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

lactation_summary_all <- read.csv(here("data", "lactation_summary_all.csv"))

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

lactation_summary <- lactation_summary_all %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         AniId = clean_ani(AniId),
         LacCalvingDate = as.Date(LacCalvingDate))

insemination <- insemination %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         InsDate = as.Date(InsDate))

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
  mutate(national_number = clean_ani(national_number))

lactation_summary <- lactation_summary %>%
  left_join(dairy_meta_farm1 %>%
              select(national_number, birth_date),
            by = c("AniLifeNumber" = "national_number"))

# Checks
dim(lactation_summary_all)
dim(lactation_summary)
colnames(lactation_summary) # Ensure LacId appears
sum(is.na(lactation_summary$LacId))


# Join lactation and insemination data to have one row per insemination attempt for each lactation cycle for each cow.

insem_lactation <- insemination %>%
  left_join(lactation_summary, by = c("AniLifeNumber", "InsLacId" = "LacId")) %>%
  select(-c(InsItyId, InsDprId, InsEdiStatus, InsRowTimeStamp))

# Checks
dim(insem_lactation)
colnames(insem_lactation)

# Join lactation and insemination data to the pregnancy data
insem_lac_preg <- insem_lactation %>%
  left_join(pregnancy, by = c("InsId" = "PreInsId"), relationship = "many-to-many")

# Checks
dim(insem_lac_preg)
colnames(insem_lac_preg)
View(insem_lac_preg %>% filter(AniLifeNumber == "FR4404288134")) # visual exam

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
preg_confirmed <- insem_lac_preg %>%
  filter(!is.na(PreDate), !is.na(LacCalvingDate))

preg_successful <- preg_confirmed %>%
  group_by(AniLifeNumber, LacCalvingDate) %>%
  arrange(PreDate) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(successful_pregnancy = TRUE) %>%
  select(InsId, successful_pregnancy) # Only keep flag and InsId for join

insem_lac_preg <- insem_lac_preg %>%
  left_join(preg_successful, by = "InsId") %>%
  mutate(successful_pregnancy = if_else(is.na(successful_pregnancy), FALSE, successful_pregnancy)) %>%
  select(-c(InsSequenceNumber, InsChargeNumber, InsConId))


# Save the de-duped dataset (insemination * lactation_summary * pregnancy)
write.csv(insem_lac_preg, here("data", "insem_lac_preg.csv"))
saveRDS(insem_lac_preg, file = here::here("data", "insem_lac_preg.rds"))
