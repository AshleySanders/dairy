# Title: Missing Milk Production Data Analysis
# Description: This script analyzes missing milk production data for cows that have calved but do not have milk production records post-calving. It joins various datasets, cleans the data, and identifies cows with missing milk records after calving, along with their exit information.
# Author: Ashley Sanders
# Created: 2024-06-24
# Updated: 2024-06-24
# Inputs:
#   - Lely SQL database: RemLactation, PrmMilkDayProduction, HemAnimal
#   - Local CSV: missing_milk_after_calving.csv
# Outputs:
#   - Final data frame: missing_milk_with_exit
#   - Optional CSV export for review


# Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

# Load milk production data from Lely
milk_all <- dbGetQuery(lely, "
  SELECT
    MdpId,
    MdpAniId,
    MdpProductionDate,
    MdpDayProduction,
    MdpDayProductionMAvg,
    MdpMilkings,
    MdpFatPercentage,
    MdpProteinPercentage
  FROM PrmMilkDayProduction
  ORDER BY MdpAniId, MdpProductionDate
")


# Load animal metadata
HemAnimal <- dbGetQuery(lely, "
  SELECT
    AniId,
    AniLifeNumber,
    AniActive,
    AniGenId,
    AniBirthday
  FROM HemAnimal
  ORDER BY AniId")

# Load lactation records
lactation <- dbGetQuery(lely, "
  SELECT
    LacId,
    LacAniId,
    LacNumber,
    LacDryOffDate,
    LacCalvingDate,
    LacColostrumDate,
    LacRemarks
  FROM RemLactation
")

# Join milk data to cow identity info
milk_cows <- milk_all %>%
  left_join(
    HemAnimal %>% select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday),
    by = c("MdpAniId" = "AniId")
  )

# Join lactation data to milk records by cow ID (not yet filtered by lactation period)
lactation_milk <- lactation %>%
  filter(LacNumber > 0, !is.na(LacCalvingDate)) %>%
  left_join(
    milk_all, by = c("LacAniId" = "MdpAniId"))

# Add animal info to the lactation+milk joined data
lactation_prod <- lactation_milk %>%
  left_join(HemAnimal %>% select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday),
            by = c("LacAniId" = "AniId"))


### --- Identify cows with calving but no milk production data post-calving
# This logic is performed in SQL due to memory limitations in R.
sql <- readLines("sql/missing_milk.sql") %>% paste(collapse = "\n")
missing_milk <- dbGetQuery(lely, sql)

missing_milk <- read.csv(here("data", "missing_milk_after_calving.csv"))

# Clean and standardize AniLifeNumber to match animal_history format
missing_milk_cleaned <- missing_milk %>%
  filter(!is.na(AniLifeNumber)) %>%                   # Skip rows with missing ID
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
