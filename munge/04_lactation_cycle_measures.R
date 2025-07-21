# Calculate measures by lactation cycle:
# - age_at_calving
# - Total number of artificial insemination attempts
# - Number of failed artificial insemination attempts
# - Ratio: successful / total insemination attempts
# - Number of failed pregnancies
# - Total Milk Yield (liters)
# - avg_daily_yield
# - early_lactation_yield : first 30 days
# - mid_lactation_yield : subsequent 60 days
# - delta_early_mid_yield : Difference between the first 30 days to next 60 days of milk production (liters)
# - Avg percent fat
# - Avg percent protein
# - is_final_cycle : Flag for last lactation cycle (?)
# - calving_to_first_insem_days
# - endmilk_to_exit_days

# Consider calculating: days_to_peak_yield


# Other variables to keep for correlation analysis
# - lactation_duration
# - dry_off_interval
# - LacCalvingDate
# - milk_production_start_date
# - milk_production_end_date
# - LacColostrumDate

# Identifiers to keep
# - AniId
# - AniLifeNumber
# - InsId
# - LacId
# - CalculatedLactationCycle
# - RemLactation_LacNumber

# Variables to consider joining from other data sets
# - exit_date
# - exit_code (exit information for correlation analysis with failed inseminations and failed pregnancies)
# - slaughter date (for comparison with last milk_production_end_date to determine if the farmer fattened the cow before the sale to the Abbatoir => add a flag for "fattened" so we can compare profitability of this strategy)

# NOTE: The insem_lac_preg dataset does NOT show multiple insemination attempts that never resulted in a pregnancy. This information should also be examined to create a complete picture for each cow, beyond lactation cycles

# Data :
# - lactation_summary_all_LacId (plus AniBirthday) from munge/02-reproduction.R.
# - data/insem_lac_preg.rds

# Load libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

# Load the cow_features table created in munge/02_cow_features_construction.R
insem_lac_preg <- readRDS(here::here("data", "insem_lac_preg.rds"))

# Calculate age at calving for each lactation cycle
age_at_calving <- lactation_summary %>%
  filter(RemLactation_LacNumber > 0, !is.na(birth_date),
         !is.na(LacCalvingDate), !is.na(LacId)) %>%
  mutate(
    age_at_calving = interval(birth_date, LacCalvingDate) %/% months(1)) %>%
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
  left_join(
    dairy_meta_farm1 %>%
      select(national_number, exit_date, exit_code),
    by = c("AniLifeNumber" = "national_number")
  )

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

# Calculate the number of days between last lactation (milk_production_end_date) and exit_date for culled cows
lactation_metrics <- lactation_metrics %>%
  group_by(AniLifeNumber) %>%
  mutate(
    endmilk_to_exit_days = if_else(
      last_lactation == TRUE & !is.na(exit_date),
      as.numeric(as.Date(exit_date) - as.Date(milk_production_end_date)),
      NA_real_
    )
  ) %>%
  ungroup()

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


