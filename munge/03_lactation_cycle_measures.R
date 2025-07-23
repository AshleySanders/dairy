# ─────────────────────────────────────────────────────────────────────────────
# Script Name: 03-lactation_metrics.R
# Author: Ashley Sanders
# Date Created: 2025-07-10
# Last Updated: 2025-07-23
# Project: Herd Management Strategy Analysis – Dairy Cow Lactation Cycle Metrics
#
# Description:
# This script generates per-lactation-cycle metrics for dairy cows using joined
# lactation, insemination, and reproductive outcome data. It computes performance
# and decision-related indicators used in profitability modeling and cluster analysis.
#
# Key Metrics Calculated (by lactation cycle):
# - age_at_calving (months)
# - n_insem: number of artificial insemination attempts
# - n_failed_insem: number of failed insemination attempts
# - n_failed_pregnancies: number of pregnancies that failed
# - successful/total insemination ratio (implied via n_insem, n_failed_insem)
# - calving_to_insem_days: days from calving to first insemination
# - calving_interval_days: time between calvings
# - dry_off_interval: time between milk end and next cycle
# - last_lactation: flag for last lactation cycle (based on exit data)
#
# Milk Production Metrics (imported from SQL & estimated delivery):
# - total_milk_production, avg_daily_yield
# - early_lactation_yield, mid_lactation_yield
# - delta_early_mid_yield
# - mean_fat_percent, mean_protein_percent
# - Estimated delivered milk using 0.959 correction factor for retained milk
#
# Identifiers Retained:
# - AniId, AniLifeNumber, LacId, InsId, CalculatedLactationCycle
#
# Inputs:
# - lactation_summary (from SQL output; contains milk metrics and dates)
# - insem_lac_preg.rds (insemination and calving data, per cycle)
# - HemAnimal (AniBirthday used for age calculations)
#
# Outputs:
# - lactation_metrics: enhanced lactation-cycle level dataset cached to disk
#
# Dependencies:
# - dplyr, lubridate, tidyr, stringr
#
# Notes:
# - still_milking flag recalculated using exit_date and last milk_production_end_date
# - Some failed inseminations not shown in insem_lac_preg; deeper reproductive record inspection needed
# - Potential future enhancements: days_to_peak_yield
# ─────────────────────────────────────────────────────────────────────────────


# Load libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

# Load necessary data
# insem_lac_preg is alreadyy in cache
# lactation_summary is already in cache

# Determine if the cow is likely still milking, clean id numbers, and calculate estimated conserved and delivered milk quantities
lactation_summary <- lactation_summary %>%
  mutate(
    AniLifeNumber = clean_ani(AniLifeNumber),
    AniMotherLifeNumber = clean_ani(AniMotherLifeNumber),
    still_milking = if_else(
      is.na(exit_date) & milk_production_end_date %in% as.Date(c("2024-09-18", "2024-09-19")),
      TRUE,
      FALSE
      ),
    est_deliver_total_milk_L = total_milk_production * 0.959,
    est_deliver_avg_daily_yield = avg_daily_yield * 0.959,
    est_deliver_early_lactation_yield = early_lactation_yield * 0.959,
    est_deliver_mid_lactation_yield = mid_lactation_yield * 0.959,
    est_deliver_delta_lactation_yield = delta_early_mid_yield * 0.959
  )

# Calculate age at calving for each lactation cycle
age_at_calving <- lactation_summary %>%
  filter(RemLactation_LacNumber > 0, !is.na(AniBirthday),
         !is.na(LacCalvingDate), !is.na(LacId)) %>%
  mutate(
    age_at_calving = interval(AniBirthday, LacCalvingDate) %/% months(1)) %>%
  select(AniLifeNumber, LacId, age_at_calving)

age_at_calving_dedup <- age_at_calving %>%
  group_by(AniLifeNumber, LacId) %>%
  summarise(age_at_calving = first(age_at_calving), .groups = "drop")

lactation_metrics <- lactation_summary %>%
  left_join(age_at_calving_dedup, by = c("AniLifeNumber", "LacId"))


# Number of inseminations per lactation cycle
insem_per_lac <- insemination %>%
  group_by(AniLifeNumber, InsLacId) %>%
  summarise(n_insem = max(InsNumber), .groups = "drop")

lactation_metrics <- lactation_metrics %>%
  left_join(insem_per_lac,
            by = c("AniLifeNumber", "LacId" = "InsLacId"))

# Checks
lactation_metrics %>%
  filter(is.na(n_insem), RemLactation_LacNumber == 1) %>%
  summarise(n_first_preg_NAs = n())

lactation_metrics %>%
  filter(is.na(n_insem), CalculatedLactationCycle == 1) %>%
  summarise(n_first_preg_calc_NAs = n())

# Examine mystery NAs in n_insem
mystery_NAs <- lactation_metrics %>%
  filter(is.na(n_insem), RemLactation_LacNumber > 1)

View(mystery_NAs) # All cows who are still lactating are missing next insemination data

# Create an indicator for first pregnancies. Create a flag to indicate why n_insem is NA
lactation_metrics <- lactation_metrics %>%
  mutate(
    first_pregnancy = RemLactation_LacNumber == 1,
    n_insem_flag = case_when(
      !is.na(n_insem) ~ "recorded",
      first_pregnancy ~ "first pregnancy",
      still_milking == 1 ~ "still milking",
      TRUE ~ "unknown"
    )
  )

# Create variable for the number of failed insemination attempts for each lactation cycle
lactation_metrics <- lactation_metrics %>%
  mutate(
    n_failed_insem = as.numeric(n_insem - 1)
  )

# Create a variable to indicate last lactation-cycles
lactation_metrics <- lactation_metrics %>%
  group_by(AniLifeNumber) %>%
  mutate(
    max_lac_if_exited = if(any(!is.na(exit_date))) {
      max(RemLactation_LacNumber[!is.na(exit_date)], na.rm = TRUE)
    }
    else {
      NA_integer_
    },
    last_lactation = if_else(
      !is.na(max_lac_if_exited) & RemLactation_LacNumber == max_lac_if_exited,
      TRUE,
      FALSE
    )
  ) %>%
  ungroup() %>%
  select(-max_lac_if_exited)

# Calculate the interval between calving and next insemination attempt
# Step 1: Calculate calving-to-insem interval per cow-lactation from insem_lac_preg
calving_to_insem <- insem_lac_preg %>%
  group_by(AniLifeNumber, InsLacId) %>%
  summarise(
    calving_to_insem_days = if (any(InsNumber == 1)) {
      as.numeric(first(InsDate[InsNumber == 1]) - first(LacCalvingDate))
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# Step 2: Join back to lactation_metrics using AniLifeNumber and InsLacId (or LacId)
lactation_metrics <- lactation_metrics %>%
  left_join(calving_to_insem, by = c("AniLifeNumber", "LacId" = "InsLacId"))

# Number of failed pregnancies per lactation cycle
failed_pregnancies <- insem_lac_preg %>%
  group_by(InsLacId) %>%
  summarise(
    n_failed_pregnancies = sum(successful_pregnancy == FALSE, na.rm = TRUE),
    .groups = "drop"
  )

# Then join this count back to lactation_metrics
lactation_metrics <- lactation_metrics %>%
  left_join(failed_pregnancies, by = c("LacId" = "InsLacId")) %>%
  mutate(
    n_failed_pregnancies = replace_na(n_failed_pregnancies, 0)
  )

# Calculate the calving interval (time between one calving and the next)
calving_interval <- lactation_metrics %>%
  group_by(AniLifeNumber) %>%
  arrange(AniLifeNumber, LacCalvingDate) %>%
  mutate(
    calving_interval_days = as.numeric(difftime(lead(LacCalvingDate), LacCalvingDate, units = "days")),
    calving_interval_reason = case_when(
      is.na(calving_interval_days) & still_milking ~ "still milking",
      is.na(calving_interval_days) ~ "no subsequent calving",
      TRUE ~ "recorded"
      )
    ) %>%
  ungroup()

calving_interval <- calving_interval %>%
  select(c(AniLifeNumber, LacCalvingDate, calving_interval_days, calving_interval_reason))

lactation_metrics <- lactation_metrics %>%
  left_join(calving_interval, by = c("AniLifeNumber", "LacCalvingDate"))

cache("lactation_metrics")