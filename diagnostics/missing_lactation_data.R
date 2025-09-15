# ------------------------------------------------------------------------------
# Script: missing_lactation_data.R
# Purpose: Identify cows with missing milk production data after calving by
#          joining lactation, milk, and animal exit data. Includes diagnostics
#          for dry-off dates, lactation length, and calving-to-insemination
#          intervals.
# Notes:   Focused on Farm1 after Lely installation (2016–2023).
# ------------------------------------------------------------------------------



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
# This will help identify if there are any lactation cycles with missing dry-off dates=
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

# Identify missing calving date data
sum(is.na(lactation_animal$LacCalvingDate)) # Total missing calving dates

# Identify cows with missing lactation_length (derived variable) between October 30, 2016 and November 19, 2023
missing_lactation_length <- lactation_animal %>%
  filter(LacNumber > 0, is.na(LacDryOffDate), LacCalvingDate > as.Date("2016-10-30") & LacCalvingDate < as.Date("2023-11-19")) %>%
  select(AniLifeNumber, LacNumber, LacCalvingDate, AniActive) %>%
  arrange(AniLifeNumber, LacNumber)

# Produce a summary table of cows who should have lactation length data but are missing it
lactation_animal %>%
  filter(LacNumber > 0, LacCalvingDate > as.Date("2016-10-30") & LacCalvingDate < as.Date("2023-11-19")) %>%
  mutate(missing_lactation_length = is.na(lactation_length_days)) %>%
  group_by(missing_lactation_length, AniActive) %>%
  summarise(count = n(), .groups = "drop")

# Summary table of cows who should have age at first calving and don't
lactation_animal %>%
  filter(LacNumber > 0, LacCalvingDate > as.Date("2016-10-30")) %>%
  mutate(missing_age_at_first_calving = is.na(age_at_first_calving)) %>%
  group_by(missing_age_at_first_calving, AniActive) %>%
  summarise(count = n(), .groups = "drop")

# Identify cows with missing calving to insemination data
missing_calving_to_insem <- insem_lactation %>%
  filter(LacNumber > 0, LacCalvingDate > as.Date("2016-10-30")) %>%
  mutate(missing_calving_to_insem = is.na(calving_to_insem)) %>%
  group_by(missing_calving_to_insem, AniActive) %>%
  summarise(count = n(), .groups = "drop")

# --- Identify cows with missing lactation dry-off data
# Step 1: Identify the last lactation per cow
last_lactation_per_cow <- insem_lactation %>%
  group_by(LacAniId) %>%
  summarise(last_lac = max(LacNumber, na.rm = TRUE), .groups = "drop")

# Step 2: Keep only last insemination per lactation
insem_last <- insem_lactation %>%
  group_by(LacAniId, LacNumber) %>%
  filter(InsNumber == max(InsNumber, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Join and remove rows corresponding to final lactation
insem_last_filtered <- insem_last %>%
  left_join(last_lactation_per_cow, by = "LacAniId") %>%
  filter(LacNumber < last_lac)  # Exclude final lactation

# Step 4: Apply calving date filter and summarize
missing_lactation_dry_days <- insem_last_filtered %>%
  filter(
    LacNumber > 0,
    as.Date(LacCalvingDate) > as.Date("2016-10-30"),
    as.Date(LacCalvingDate) < as.Date("2023-11-19")
  ) %>%
  mutate(missing_lactation_dry_days = is.na(next_lactation_dry_days)) %>%
  group_by(missing_lactation_dry_days, AniActive) %>%
  summarise(count = n(), .groups = "drop")

print(missing_lactation_dry_days)