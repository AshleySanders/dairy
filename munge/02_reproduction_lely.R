# ------------------------------------------------------------------------------
# Script Name:    02_reproduction.R
# Project:        Cockpit Agriculture – Herd Management Strategy
#
# Purpose:        Construct a clean, cow-level dataset of reproductive events,
#                 combining lactation, insemination, and pregnancy records.
#
# Description:    This script performs the following:
#                 - Cleans and standardizes animal identifiers
#                 - Attaches each insemination to a lactation cycle
#                 - Joins pregnancy confirmation data to insemination records
#                 - Deduplicates pregnancies (latest confirmation per InsId)
#                 - Flags successful inseminations (pregnancy confirmed)
#                 - Flags successful pregnancies (confirmed + led to calving)
#                 - Adds birth_date from validated Supabase metadata
#
# Inputs:
#   - Cache: insemination, pregnancy
#   - CSV:   data/lactation_summary_all.csv (from SQL script)
#   - Table: dairy_meta_farm1 (de-duplicated Supabase metadata)
#
# Outputs:
#   - data/insem_lac_preg.rds: Cleaned and flagged dataset (RDS)
#   - data/insem_lac_preg.csv: Exported copy (CSV)
#
# Author:         Ashley Sanders
# Created:        2025-06-19
# Last Updated:   2025-07-21
# ------------------------------------------------------------------------------


# Load libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)


# Data: insemination table from cache
# Data: Use lactation_summary_all generated from SQL ---

lactation_summary_all <- read.csv(here("data", "lactation_cycles_milk_metrics.csv"))

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

lactation_summary <- lactation_summary_all %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         AniId = clean_ani(AniId),
         LacCalvingDate = as.Date(LacCalvingDate))

pregnancy <- pregnancy %>%
  mutate(PreDate = as.Date(PreDate))

# Add lactation ID (LacId) from Lely RemLactation (lactation_data table) to the cleaned & validated lactation_summary_all in order to join it with insemination data

lactation_summary <- lactation_summary %>%
  mutate(AniId = as.character(AniId))

lactation_summary <- lactation_summary%>%
  mutate(
    AniLifeNumber = case_when(
      AniId == "253" ~ "FR4404288298",
      AniId == "257" ~ "FR4404288299",
      AniId == "267" ~ "FR4404288307",
      AniId == "277" ~ "FR4404288308",
      AniId == "282" ~ "FR4404288319",
      AniId == "289" ~ "FR4404288318",
      AniId == "286" ~ "FR4404288320",
      AniId == "987" ~ "FR4404288598",
      AniId == "1040" ~ "FR4404288645",
      AniId == "1064" ~ "FR4404288667",
      AniId == "1071" ~ "FR4404288645",
      TRUE ~ AniLifeNumber
    ))

lactation_summary <- lactation_summary %>%
  mutate(AniId = as.integer(AniId))

dairy_meta_farm1 <- dairy_meta_farm1 %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber))

lactation_summary <- lactation_summary %>%
  left_join(dairy_meta_farm1 %>%
              select(-AniId, AniActive),
            by = "AniLifeNumber")


# Ensure correct data types
glimpse(lactation_summary)

lactation_summary <- lactation_summary %>%
  mutate(
    LacColostrumDate = as.Date(LacColostrumDate),
    milk_production_start_date = as.Date(milk_production_start_date),
    milk_production_end_date = as.Date(milk_production_end_date),
    avg_daily_yield = as.numeric(avg_daily_yield),
    mean_fat_percent = as.numeric(mean_fat_percent),
    mean_protein_percent = as.numeric(mean_protein_percent),
    dry_off_date = as.Date(dry_off_date),
    dry_off_interval = as.integer(dry_off_interval)
  )

# Checks
dim(lactation_summary_all)
dim(lactation_summary)
colnames(lactation_summary)
sum(is.na(lactation_summary$LacId))

# De-duplicate based on LacIds
lactation_summary_dedup <- lactation_summary %>%
  arrange(desc(!is.na(AniMotherLifeNumber))) %>%  # prioritize non-NA mother IDs
  group_by(LacId) %>%
  slice(1) %>%
  ungroup()

# verify that the number of rows matches the original lactation_summary_all table
nrow(lactation_summary_all) == nrow(lactation_summary_dedup)

# After verification save the de-duped table as the original table name
lactation_summary <- lactation_summary_dedup

# Save final dataframe in cache
cache("lactation_summary")

# Join lactation and insemination data to have one row per insemination attempt for each lactation cycle for each cow.

insem_lactation <- insemination %>%
  left_join(lactation_summary %>%
              select(-c(AniBirthday, AniGenId, AniMotherLifeNumber, AniId, milk_production_start_date, milk_production_end_date, lactation_duration, total_milk_production, avg_daily_yield, early_lactation_yield, mid_lactation_yield, delta_early_mid_yield, mean_fat_percent, mean_protein_percent)),
            by = c("AniLifeNumber", "InsLacId" = "LacId"))

# Checks
dim(insem_lactation)
colnames(insem_lactation)


# Join lactation and insemination data to the pregnancy data
insem_lac_preg <- insem_lactation %>%
  left_join(pregnancy,
  by = c("InsId" = "PreInsId"), relationship = "many-to-many")

# Checks
dim(insem_lac_preg)
colnames(insem_lac_preg)
# View(insem_lac_preg %>% filter(AniLifeNumber == "FR4404288134")) # visual exam

# De-duplicate when there are multiple pregnancy confirmation dates

insem_lac_preg_dedup <- insem_lac_preg %>%
  group_by(InsId) %>%
  arrange(desc(PreDate)) %>%  # Latest confirmation first
  slice_head(n = 1) %>%
  ungroup()

dup_count <- insem_lac_preg %>%
  group_by(InsId) %>%
  filter(n() > 1) %>%
  summarise(n_duplicates = n(), .groups = "drop")

nrow(dup_count)  # Number of insemination events with multiple pregnancy records

# Check
nrow(insem_lactation) == nrow(insem_lac_preg_dedup)

# Overwrite the previous join now that it has been de-duped and validated
insem_lac_preg <- insem_lac_preg_dedup

insem_lac_preg_flagged <- insem_lac_preg %>%
  mutate(successful_insem = if_else(!is.na(PreDate), TRUE, FALSE))

# after validation, save as original table
insem_lac_preg <- insem_lac_preg_flagged

# identify failed pregnancy after confirmation
insem_lac_preg <- insem_lac_preg %>%
  mutate(
    successful_pregnancy = !is.na(PreDate) & !is.na(LacCalvingDate)
  )

# Save the de-duped dataset (insemination * lactation_summary * pregnancy)
write.csv(insem_lac_preg, here("data", "insem_lac_preg.csv"))
saveRDS(insem_lac_preg, file = here::here("data", "insem_lac_preg.rds"))
cache("insem_lac_preg")
