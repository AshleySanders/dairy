# -----------------------------------------------------------------------------
# Script Name: 05_cow_features_build_and_combine.R
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
dmet <- get(paste0(farm_prefix, "_dairy_meta")) # now contains slaughter details

# animals_slaughter uses 'national_number', align the key name here:
if (!"AniLifeNumber" %in% names(dmet) && "national_number" %in% names(dmet)) {
  dmet <- dplyr::rename(dmet, AniLifeNumber = national_number)
}

# --- Valid cows in scope ------------------------------------------------------
valid_cow_ids <- unique(lm$AniLifeNumber)

# --- Exit-related features ----------------------------------------------------
exit_age <- lm %>%
  # keep only the final lactation where the cow actually exited
  dplyr::filter(last_lactation == TRUE, !is.na(exit_date)) %>%
  dplyr::transmute(
    AniLifeNumber,
    # calculate age at exit in whole months
    age_at_exit = interval(as.Date(AniBirthday), as.Date(exit_date)) %/% months(1)
  ) %>%
  distinct()

# Compute number of days between last milking date and exit date
endmilk_to_exit <- lm %>%
  # Keep only the last lactation cycle where the cow actually exited
  dplyr::filter(last_lactation == TRUE, !is.na(exit_date)) %>%
  # Compute the days from end of milking to exit
  dplyr::transmute(
    AniLifeNumber,
    endmilk_to_exit_days = as.numeric(exit_date - milk_production_end_date)
  ) %>%
  # In case there is more than one record per cow (shouldn't normally happen), keep unique
  group_by(AniLifeNumber) %>%
  summarise(
    endmilk_to_exit_days = max(endmilk_to_exit_days, na.rm = TRUE),  # or min/first
    .groups = "drop"
  )


# --- Aggregations from lactation metrics --------------------------------------
cow_lactation_summary <- lm %>%
  dplyr::group_by(AniLifeNumber) %>%
  dplyr::summarise(
    # milk production
    avg_early_lactation_yield  = mean(early_lactation_yield, na.rm = TRUE),
    avg_mid_lactation_yield    = mean(mid_lactation_yield, na.rm = TRUE),
    avg_delta_early_mid_yield  = mean(delta_early_mid_yield, na.rm = TRUE),
    avg_daily_yield            = mean(avg_daily_yield, na.rm = TRUE),
    avg_total_milk             = mean(total_milk_production, na.rm = TRUE),

    # reproductive metrics
    avg_insem                  = mean(n_insem, na.rm = TRUE),
    avg_failed_insem           = mean(n_failed_insem, na.rm = TRUE),
    avg_failed_pregnancies     = mean(n_failed_pregnancies, na.rm = TRUE),
    avg_calving_to_insem       = mean(calving_to_insem_days, na.rm = TRUE),

    # lifecycle metrics
    number_lactations          = max(pmax(RemLactation_LacNumber,
                                          CalculatedLactationCycle), na.rm = TRUE),
    avg_lactation_duration     = mean(lactation_duration, na.rm = TRUE),
    avg_dry_interval           = mean(dry_off_interval, na.rm = TRUE),
    n_intervals                = sum(!is.na(dry_off_interval)),
    age_at_first_calving       = min(age_at_calving, na.rm = TRUE),
    latest_lactation_date      = max(milk_production_start_date, na.rm = TRUE),

    # animal descriptors
    AniBirthday                = dplyr::first(AniBirthday),
    AniGenId                   = dplyr::first(AniGenId),
    .groups = "drop"
  )

# Create a column for birth_year and identify quartile cut points
cow_lactation_summary <- cow_lactation_summary %>% mutate(birth_year = year(AniBirthday))
summary(cow_lactation_summary$birth_year)


# --- Insemination summaries ---------------------------------------------------
cow_insem_summary <- lm %>%
  dplyr::group_by(AniLifeNumber) %>%
  dplyr::summarise(
    total_insem         = sum(coalesce(n_insem, 0L), na.rm = TRUE),
    total_successes     = sum(coalesce(n_insem, 0L) - coalesce(n_failed_insem, 0L), na.rm = TRUE),
    insem_success_ratio = if_else(total_insem > 0, total_successes / total_insem, NA_real_),
    .groups = "drop"
  )

cow_age_at_first_insem <- ilp %>%
  dplyr::arrange(AniLifeNumber, InsDate) %>%
  dplyr::group_by(AniLifeNumber) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    age_at_first_insem = (interval(as.Date(AniBirthday), as.Date(InsDate)) %/% months(1)) - 12,
    first_insem_date   = InsDate
  ) %>%
  dplyr::select(AniLifeNumber, age_at_first_insem, InsDate) %>%
  dplyr::filter(AniLifeNumber %in% valid_cow_ids)

cow_age_first_success_insem <- ilp %>%
  dplyr::filter(successful_insem == TRUE) %>%
  dplyr::arrange(AniLifeNumber, InsDate) %>%
  dplyr::group_by(AniLifeNumber) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    age_first_successful_insem = (interval(AniBirthday, InsDate) %/% months(1)) - 12
  ) %>%
  dplyr::select(AniLifeNumber, age_first_successful_insem) %>%
  dplyr::filter(AniLifeNumber %in% valid_cow_ids)


#- -- Join everything into cow_features ----------------------------------------
farm_cow_features <- cow_lactation_summary %>%
  dplyr::left_join(
    dmet %>% dplyr::select(-c(AniId, AniBirthday, AniMotherLifeNumber, date, customer_id)),
    by = c("AniLifeNumber" = "national_number")
  ) %>%
  dplyr::left_join(exit_age,                        by = "AniLifeNumber") %>%
  dplyr::left_join(endmilk_to_exit,                 by = "AniLifeNumber") %>%
  dplyr::left_join(cow_insem_summary,               by = "AniLifeNumber") %>%
  dplyr::left_join(cow_age_at_first_insem,          by = "AniLifeNumber") %>%
  dplyr::left_join(cow_age_first_success_insem,     by = "AniLifeNumber") %>%
  dplyr::left_join(chs,                             by = "AniLifeNumber") %>%
  dplyr::mutate(
    n_health_problems = tidyr::replace_na(n_health_problems, 0),
    recovery_duration = tidyr::replace_na(recovery_duration, 0)
  ) %>%
  dplyr::mutate(
    birth_year = lubridate::year(AniBirthday),
    cohort = dplyr::case_when(
      birth_year < 2013                       ~ "pre-2013",
      birth_year >= 2013 & birth_year <= 2016 ~ "2013-2016",
      birth_year >= 2016 & birth_year <= 2019 ~ "2016-2019",
      birth_year >= 2019                      ~ "2019+",
      TRUE                                    ~ NA_character_
    ),
    farm = farm_id   # <-- provenance column
  )

#--- assign + cache farm-specific table (e.g., fm5_cow_features) ---------------
assign(paste0(farm_prefix, "_cow_features"), farm_cow_features, envir = .GlobalEnv)
cache(paste0(farm_prefix, "_cow_features"))
readr::write_rds(farm_cow_features, here::here("data", paste0(farm_prefix, "_cow_features.rds")))
readr::write_csv(farm_cow_features, here::here("data", paste0(farm_prefix, "_cow_features.csv")))

# --- Build/append the combined multi-farm cow_features ------------------------
# Prefer an in-memory 'cow_features' if present; else fall back to fm1_cow_features.
existing_cf <- if (!is.null(get0("cow_features"))) {
  get("cow_features")
} else if (!is.null(get0("fm1_cow_features"))) {
  get("fm1_cow_features")
} else {
  NULL
}

# Ensure existing has a 'farm' column; if it's fm1-only and missing, tag it.
if (!is.null(existing_cf) && !"farm" %in% names(existing_cf)) {
  # If this table is known to be fm1, label it as such
  existing_cf <- dplyr::mutate(existing_cf, farm = "farm1")
}


# Bind + keep consistent column order (dplyr will pad missing columns with NA)
combined_cf <- if (is.null(existing_cf)) {
  farm_cow_features
} else {
  # ensure both have same col set
  all_cols <- union(names(existing_cf), names(farm_cow_features))
  dplyr::bind_rows(
    dplyr::select(existing_cf, dplyr::any_of(all_cols)),
    dplyr::select(farm_cow_features, dplyr::any_of(all_cols))
  ) %>%
    dplyr::select(dplyr::all_of(all_cols))
}

# Write back the combined table as 'cow_features'
assign("cow_features", combined_cf, envir = .GlobalEnv)
cache("cow_features")
readr::write_rds(combined_cf, here::here("data", "cow_features.rds"))
readr::write_csv(combined_cf, here::here("data", "cow_features.csv"))
