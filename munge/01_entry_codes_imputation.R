# ------------------------------------------------------------------------------
# Script Name:    01_entry_codes_imputation.R
# Project:        Cockpit Agriculture – Herd Management Strategy
# Purpose:        Impute missing entry codes and dates for cows born into the herd
#
# Description:    This script updates the `animals_meta_farm1` dataset by:
#                 - Identifying cows born into the herd (mother also in herd)
#                 - Imputing missing `entry_code` as "N" for these cases
#                 - Imputing `entry_date` with the cow’s `AniBirthday` when missing
#
# Inputs:
#   - animals_meta_farm1 (from cached data)
#
# Outputs:
#   - animals_meta_farm1 (updated in the global environment)
#
# Author:         Ashley Sanders
# Created:        2025-07-17
# Last updated:   2025-07-17
# ------------------------------------------------------------------------------

# Identify all AniLifeNumbers in the herd
herd_ids <- unique(animals_meta_farm1$AniLifeNumber)

# Add entry code "N" only if entry_code is currently NA and mother is in the herd
animals_meta_farm1 <- animals_meta_farm1 %>%
  mutate(
    entry_code = if_else(
      is.na(entry_code) & AniMotherLifeNumber %in% herd_ids,
      "N",
      entry_code
    )
  )

# Impute entry_date with AniBirthday if entry_code is "N" and entry_date is missing
animals_meta_farm1 <- animals_meta_farm1 %>%
  mutate(
    entry_date = if_else(
      is.na(entry_date) & entry_code == "N",
      AniBirthday,
      entry_date
    )
  )

# ------------------------------------------------------------------------------
# Quick Checks
# ------------------------------------------------------------------------------

# Check distribution of entry_code
table(animals_meta_farm1$entry_code)

# Count missing values
cat("Missing entry_code: ", sum(is.na(animals_meta_farm1$entry_code)), "\n")
cat("Missing entry_date: ", sum(is.na(animals_meta_farm1$entry_date)), "\n")
cat("Missing exit_code: ", sum(is.na(animals_meta_farm1$exit_code)), "\n")
cat("Missing exit_date: ", sum(is.na(animals_meta_farm1$exit_date)), "\n")

# Check active animals
table(animals_meta_farm1$AniActive)
