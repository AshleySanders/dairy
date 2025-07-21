# ------------------------------------------------------------------------------
# Script Name:    farm1_fix_hemanimal_ids.R
# Project:        Cockpit Agriculture – Herd Management Strategy
# Purpose:        Manually correct known AniLifeNumber errors in HemAnimal data
#                 specific to Farm 1, based on verified national ID mismatches.
#
# Description:    This script applies farm-specific corrections to HemAnimal,
#                 ensuring accurate joins with other Lely and Supabase tables.
#
# Notes:          - Only used for Farm 1
#                 - Must be sourced *after* loading HemAnimal, before joins
#                 - Re-save HemAnimal to cache after applying this script
#
# Author:         Ashley Sanders
# Created:        2025-07-21
# ------------------------------------------------------------------------------


# Manually clean misidentified cow's national numbers from Lely data for Farm1
HemAnimal <- HemAnimal %>%
  mutate(AniId = as.character(AniId))

HemAnimal <- HemAnimal %>%
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
    ),
    AniBirthday = case_when(
      AniLifeNumber == "FR4404288300" ~ as.Date("2017-08-20"),
      AniLifeNumber == "FR4404288320" ~ as.Date("2017-12-03"),
      AniLifeNumber == "FR4404288307" ~ as.Date("2017-09-12"),
      AniLifeNumber == "FR4404288319" ~ as.Date("2017-11-14"),
      TRUE ~ AniBirthday
    ),
    AniMotherLifeNumber = case_when(
      AniLifeNumber == "FR4404288681" ~ "FR4404288555",
      TRUE ~ AniMotherLifeNumber
    )
  )


# Quick check before caching.
HemAnimal %>%
  filter(AniId %in% c("253", "257", "267", "277", "282", "289", "286", "987", "1040", "1064", "1071") |
           AniLifeNumber %in% c("FR4404288300", "FR4404288320", "FR4404288307", "FR4404288319", "FR4404288415", "FR4404288681")) %>%
  select(AniId, AniLifeNumber, AniBirthday, AniMotherLifeNumber)


# Manual corrections for dairy_meta_farm1
dairy_meta_farm1 <- dairy_meta_farm1 %>%
  mutate(
    exit_date = case_when(
      national_number == "FR4404288320" ~ as.Date("2023-04-11"),
      national_number =="FR4404288415" ~ as.Date("2023-07-31"),
      TRUE ~ exit_date
      ),
    exit_code = case_when(
      national_number == "FR4404288320" ~ "B",
      national_number == "FR4404288415" ~ "B",
      TRUE ~ exit_code))