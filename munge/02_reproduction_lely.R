# ------------------------------------------------------------------------------
# Script Name:    02_reproduction.R
# Project:        Cockpit Agriculture – Herd Management Strategy
# Purpose:        Join insemination, lactation, and pregnancy data to create a
#                 clean, de-duplicated dataset for reproductive event analysis.
#
# Description:    This script prepares cow-level reproductive records by:
#                 - Cleaning identifiers and date fields
#                 - Attaching lactation IDs to validated calving events
#                 - Joining insemination records to lactations
#                 - Matching pregnancies to inseminations (deduping on PreDate)
#                 - Flagging each insemination as successful (if confirmed)
#                 - Flagging pregnancies as successful (if result in calving)
#
# Inputs:
#   - Lely SQL tables: RemInsemination, RemPregnancy, RemLactation (for LacId)
#   - data/lactation_summary_all.csv (validated summary from SQL)
#
# Outputs:
#   - data/insem_lac_preg.rds: Cleaned, deduplicated, and flagged dataset
#   - data/insem_lac_preg.csv: Exported copy for inspection or sharing
#
# Author:         Ashley Sanders
# Created:        2025-06-19
# Last updated:   2025-07-10
# ------------------------------------------------------------------------------


# Load libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)

# Run munge/01_save_sql_tables.R to load the Lely SQL tables

# Data: insemination table from Lely (run 01_save_sql_tables.R)
# Data: Use lactation_summary_all generated from SQL ---

lactation_summary_all <- read.csv(here("data", "lactation_summary_all.csv"))

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

lactation_summary_all <- lactation_summary_all %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         LacAniId = clean_ani(CowID),
         LacCalvingDate = as.Date(LacCalvingDate)) %>%
  select(-CowID)

insemination <- insemination %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         InsDate = as.Date(InsDate))

lactation_data_clean <- lactation_data %>%
  mutate(LacAniId = clean_ani(LacAniId),
         LacCalvingDate = as.Date(LacCalvingDate))

pregnancy <- pregnancy %>%
  mutate(PreDate = as.Date(PreDate))

# Add lactation ID (LacId) from Lely RemLactation (lactation_data table) to the cleaned & validated lactation_summary_all in order to join it with insemination data
lactation_summary_all_LacId <- lactation_summary_all %>%
  left_join(
    lactation_data_clean %>%
      select(LacAniId, LacCalvingDate, LacId, LacRemarks, LacColostrumDate),
    by = c("LacAniId", "LacCalvingDate" = "LacCalvingDate")
  )

# Checks
dim(lactation_summary_all)
dim(lactation_summary_all_LacId)
colnames(lactation_summary_all_LacId) # Ensure LacId appears
sum(is.na(lactation_summary_all_LacId$LacId))


# Join lactation and insemination data to have one row per insemination attempt for each lactation cycle for each cow.

insem_lactation <- insemination %>%
  left_join(lactation_summary_all_LacId, by = c("AniLifeNumber", "InsLacId" = "LacId")) %>%
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
  mutate(successful_pregnancy = if_else(is.na(successful_pregnancy), FALSE, successful_pregnancy))


# Save the de-duped dataset (insemination * lactation_summary * pregnancy)
write.csv(insem_lac_preg, here("data", "insem_lac_preg.csv"))
saveRDS(insem_lac_preg, file = here::here("data", "insem_lac_preg.rds"))
