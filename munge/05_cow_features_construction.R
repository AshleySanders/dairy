# -----------------------------------------------------------------------------
# Script Name: 06_cow_features_build_and_combine.R
# Author: Ashley Sanders
# Date Created: 2025-08-28
# Last Updated: 2025-08-28
# Project: Herd Management Strategy Analysis – Cow-Level Feature Construction
#
# Description:
# 1) Builds a per-cow feature table for the active farm (via farm_prefix).
# 2) Appends (row-binds) the new farm’s features to the existing single-farm
#    table (originally from fm1), adding a `farm` column for provenance.
#
# Inputs (farm-scoped, cached earlier):
# - <farm>_lactation_metrics
# - <farm>_insem_lac_preg
# - <farm>_cow_health_summary
# - <farm>_dairy_meta
# - <farm>_animals_slaughter  (join key may be AniLifeNumber or national_number)
#
# Also expected in-memory:
# - fm1_cow_features (original single-farm features you already saved)
#   OR cow_features (prior table) – script will try both.
#
# Outputs:
# - <farm>_cow_features (cached, farm-specific)
# - data/<farm>_cow_features.rds
# - data/<farm>_cow_features.csv
# - cow_features (combined multi-farm), cached + files
#
# Dependencies:
# - dplyr, tidyr, lubridate, readr (via tidyverse)
# - ProjectTemplate::cache()
# - farm config + helpers (farm_prefix, clean_ani, etc.)
# -----------------------------------------------------------------------------

# Load config and helpers
source(here::here("config", "farm5_config.R"))
source(here::here("lib", "helpers.R"))

# --- Pull farm-scoped tables --------------------------------------------------
lm   <- get(paste0(farm_prefix, "_lactation_metrics"))
ilp  <- get(paste0(farm_prefix, "_insem_lac_preg"))
chs  <- get(paste0(farm_prefix, "_cow_health_summary"))
dmet <- get(paste0(farm_prefix, "_dairy_meta"))
asl  <- get(paste0(farm_prefix, "_animals_slaughter"))

# animals_slaughter uses 'national_number', align the key name here:
if (!"AniLifeNumber" %in% names(asl) && "national_number" %in% names(asl)) {
  asl <- dplyr::rename(asl, AniLifeNumber = national_number)
}

# --- Valid cows in scope ------------------------------------------------------
valid_cow_ids <- unique(lm$AniLifeNumber)



# Compute age_at_exit
exit_age <- lactation_metrics %>%
  # keep only the final lactation where the cow actually exited
  filter(last_lactation == TRUE, !is.na(exit_date)) %>%
  transmute(
    AniLifeNumber,
    # calculate age at exit in whole months
    age_at_exit = interval(as.Date(AniBirthday), as.Date(exit_date)) %/% months(1)
  ) %>%
  distinct()

# Compute number of days between last milking date and exit date
endmilk_to_exit <- lactation_metrics %>%
  # Keep only the last lactation cycle where the cow actually exited
  filter(last_lactation == TRUE, !is.na(exit_date)) %>%
  # Compute the days from end of milking to exit
  transmute(
    AniLifeNumber,
    endmilk_to_exit_days = as.numeric(exit_date - milk_production_end_date)
  ) %>%
  # In case there is more than one record per cow (shouldn't normally happen), keep unique
  distinct()

# Calculate aggregate per cow features from lactation_metrics

cow_lactation_summary <- fm5_lactation_metrics %>%
  group_by(AniLifeNumber) %>%
  summarise(
    # milk production
    avg_early_lactation_yield = mean(early_lactation_yield, na.rm = TRUE),
    avg_mid_lactation_yield  = mean(mid_lactation_yield, na.rm = TRUE),
    avg_delta_early_mid_yield = mean(delta_early_mid_yield, na.rm = TRUE),
    avg_daily_yield           = mean(avg_daily_yield, na.rm = TRUE),
    avg_total_milk            = mean(total_milk_production, na.rm = TRUE),

    # reproductive metrics
    avg_insem                 = mean(n_insem, na.rm = TRUE),
    avg_failed_insem          = mean(n_failed_insem, na.rm = TRUE),
    avg_failed_pregnancies    = mean(n_failed_pregnancies, na.rm = TRUE),
    avg_calving_to_insem      = mean(calving_to_insem_days, na.rm = TRUE),

    # lifecycle metrics
    number_lactations         = max(pmax(RemLactation_LacNumber, CalculatedLactationCycle), na.rm = TRUE),
    avg_lactation_duration    = mean(lactation_duration, na.rm = TRUE),
    avg_dry_interval          = mean(dry_off_interval, na.rm = TRUE),
    n_intervals               = sum(!is.na(dry_off_interval)),
    age_at_first_calving      = min(age_at_calving, na.rm = TRUE),
    latest_lactation_date     = max(milk_production_start_date, na.rm = TRUE),

    # animal descriptors (will be recycled per group)
    AniBirthday               = first(AniBirthday),
    AniGenId                  = first(AniGenId),

    .groups = "drop"
  )


# Calculate the artificial insemination success ratio per cow:
cow_insem_summary <- lactation_metrics %>%
  group_by(AniLifeNumber) %>%
  summarise(
    total_insem        = sum(coalesce(n_insem, 0L), na.rm = TRUE),
    total_successes    = sum(coalesce(n_insem, 0L) -
                               coalesce(n_failed_insem, 0L), na.rm = TRUE),
    insem_success_ratio = if_else(total_insem > 0,
                                  total_successes / total_insem,
                                  NA_real_),
    .groups = "drop"
  )

cow_age_at_first_insem <- insem_lac_preg %>%
  arrange(AniLifeNumber, InsDate) %>%
  group_by(AniLifeNumber) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    age_at_first_insem = interval(as.Date(AniBirthday), as.Date(InsDate)) %/% months(1),
    first_insem_date = InsDate
  ) %>%
  select(c(AniLifeNumber, age_at_first_insem, InsDate)) %>%
  filter(AniLifeNumber %in% valid_cow_ids)

cow_age_first_success_insem <- insem_lac_preg %>%
  filter(successful_insem == TRUE) %>%
  arrange(AniLifeNumber, InsDate) %>%
  group_by(AniLifeNumber) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    age_first_successful_insem = interval(AniBirthday, InsDate) %/% months(1)
  ) %>%
  select(AniLifeNumber, age_first_successful_insem) %>%
  filter(AniLifeNumber %in% valid_cow_ids)


# Join everything into cow_features ---
cow_features <- cow_lactation_summary %>%
  left_join(dairy_meta_farm1 %>%
              select(-c(AniId, AniGenId, AniBirthday, AniActive, AniMotherLifeNumber, date, customer_id)), by = "AniLifeNumber") %>%
  left_join(exit_age, by = "AniLifeNumber") %>%
  left_join(endmilk_to_exit, by = "AniLifeNumber") %>%
  left_join(cow_insem_summary, by = "AniLifeNumber") %>%
  left_join(cow_age_at_first_insem, by = "AniLifeNumber") %>%
  left_join(cow_age_first_success_insem, by = "AniLifeNumber") %>%
  left_join(cow_health_summary, by = "AniLifeNumber") %>%
  left_join(animals_slaughter_farm1, by = "AniLifeNumber")

# Replace NAs with 0s for 2 variables that come from cow_health_summary
cow_features <- cow_features %>%
  mutate(
    n_health_problems = replace_na(n_health_problems, 0),
    recovery_duration = replace_na(recovery_duration,   0)
  )

# Create cohorts
cow_features <- cow_features %>%
  mutate(
    # 1. Extract the birth year
    birth_year = year(AniBirthday),

    # 2. Assign to cohort buckets
    cohort = case_when(
      birth_year < 2016                    ~ "pre-2016",
      birth_year >= 2016 & birth_year <= 2018 ~ "2016-2018",
      birth_year >= 2019 & birth_year <= 2021 ~ "2019-2021",
      birth_year >= 2022                   ~ "2022+",
      TRUE                                  ~ NA_character_
    ))


# Save or View the result ---
cache("cow_features")
saveRDS(cow_features, here("data", "cow_features.rds"))
write.csv(cow_features, here("data", "cow_features.csv"), row.names = TRUE)

