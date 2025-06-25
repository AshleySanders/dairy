# Title: Missing Milk Production Data Analysis
# Description: This script analyzes missing milk production data for cows that have calved but do not have milk production records post-calving. It joins various datasets, cleans the data, and identifies cows with missing milk records after calving, along with their exit information.
# Author: Ashley Sanders
# Created: 2024-06-24
# Updated: 2024-06-24
# Inputs:
#   - Lely SQL database tables created in munge/01_save_sql_tables.R
#   - Local CSV: missing_milk_after_calving.csv
# Outputs:
#   - Final data frame: missing_milk_with_exit
#   - Optional CSV export for review


# Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)



# Join lactation data to milk records by cow ID (not yet filtered by lactation period)
lactation_milk <- lactation %>%
  filter(LacNumber > 0, !is.na(LacCalvingDate)) %>%
  left_join(
    milk_all, by = c("LacAniId" = "MdpAniId"))

# Laction, millk-production, and animal metadata joined
lactation_prod <- lactation_milk %>%
  left_join(
    HemAnimal %>% select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday),
    by = c("LacAniId" = "AniId")
  )

# Add animal info to the lactation (but not milk production) joined data
lactation_animal <- lactation %>%
  left_join(HemAnimal %>% select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday),
            by = c("LacAniId" = "AniId"))

# Cross-tabulate the presence of dry-off date for all of the lactation cycles reported and the animal's status in herd (active/inactive)
# This will help identify if there are any lactation cycles with missing dry-off dates
# Step 1: Filter to all valid lactation cycles
dryoff_status <- lactation_animal %>%
  filter(LacNumber > 0) %>%
  mutate(dryoff_missing = is.na(LacDryOffDate)) %>%
  select(AniLifeNumber, LacNumber, dryoff_missing, AniActive)

# Step 2: Count number of lactation cycles by dryoff status and animal activity
summary_table <- dryoff_status %>%
  count(dryoff_missing, AniActive)

print(summary_table)


# --- Count cows with at least one missing LacDryOffDate (any cycle)
# Filter lactation cycles with missing dry-off date (excluding LacNumber == 0)
dryoff_missing_any <- lactation_animal %>%
  filter(LacNumber > 0, is.na(LacDryOffDate)) %>%
  distinct(AniLifeNumber, AniActive)  # one row per cow

# Count how many active/inactive cows this applies to
summary_table <- dryoff_missing_any %>%
  count(AniActive)

print(summary_table)

# --- Identify cows with calving but no LacDryOffDate post-calving
missing_dryoff_calvings <- lactation_animal %>%
  filter(LacNumber > 0, is.na(LacDryOffDate)) %>%
  select(AniLifeNumber, LacNumber, LacCalvingDate, AniActive) %>%
  arrange(AniLifeNumber, LacNumber)

View(missing_dryoff_calvings)

# --- Identify cows with calving but no LacDryOff post-calving after 2016 installation of Lely milking machine and 305 days before the date of the backup (excluding cows likely still lactating who would therefore not have a dry-off date yet)
missing_dryoff_post2016_no_recent_calf <- lactation_animal %>%
  filter(
    LacNumber > 0,
    is.na(LacDryOffDate),
    LacCalvingDate > as.Date("2016-10-30") & LacCalvingDate < as.Date("2023-11-19")
    ) %>%
  select(AniLifeNumber, LacNumber, LacCalvingDate, AniActive) %>%
  arrange(AniLifeNumber, LacNumber)

View(missing_dryoff_post2016_no_recent_calf)

write.csv(missing_dryoff_post2016_no_recent_calf, here("data", "missing_dryoff_post2016_no_recent_calf.csv"), row.names = FALSE)


### --- Identify cows with calving but no milk production data post-calving
# This logic is performed in SQL due to memory limitations in R.
sql <- readLines("sql/missing_milk.sql") %>% paste(collapse = "\n")
missing_milk <- dbGetQuery(lely, sql)

missing_milk <- read.csv(here("data", "missing_milk_after_calving.csv"))

# Clean and standardize AniLifeNumber to match animal_history format
missing_milk_cleaned <- missing_milk %>%
  filter(!is.na(AniLifeNumber)) %>%  # Skip rows with missing ID
  mutate(AniLifeNumber = str_replace_all(str_trim(as.character(AniLifeNumber)), " ", ""))

# Prepare and deduplicate animal exit data
animals_history_cleaned <- animals_history %>%
  mutate(animal = str_trim(as.character(animal))) %>%
  select(animal, exit_code, exit_date)  # Only keep relevant columns

animals_history_unique <- animals_history_cleaned %>%
  distinct(animal, .keep_all = TRUE)

# Final join: add exit info to missing milk dataset
missing_milk_with_exit <- missing_milk_cleaned %>%
  left_join(animals_history_unique, by = c("AniLifeNumber" = "animal"))

View(missing_milk_with_exit)

# Save final data set
write.csv(missing_milk_with_exit, here("data", "missing_milk_with_exit.csv"))
