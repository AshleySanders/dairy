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
age_at_calving <- lactation_summary_all_LacId %>%
  filter(RemLactation_LacNumber > 0, !is.na(AniBirthday),
         !is.na(LacCalvingDate), !is.na(LacId)) %>%
  mutate(
    age_at_calving = interval(AniBirthday, LacCalvingDate) %/% months(1)) %>%
  select(AniLifeNumber, LacId, age_at_calving)

age_at_calving_dedup <- age_at_calving %>%
  group_by(AniLifeNumber, LacId) %>%
  summarise(age_at_calving = first(age_at_calving), .groups = "drop")

lactation_metrics <- lactation_summary_all_LacId %>%
  left_join(age_at_calving_dedup, by = c("AniLifeNumber", "LacId"))

# Number of inseminations per lactation cycle
insemination <- insemination %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber))

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
animals_meta_farm1 <- animals_meta_farm1 %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber))

# Identify duplicate AniLifeNumbers
duplicate_ids <- animals_meta_farm1 %>%
  count(AniLifeNumber) %>%
  filter(n > 1) %>%
  pull(AniLifeNumber)

# View all columns for duplicated cows
filtered_animals <- animals_meta_farm1 %>%
  filter(AniLifeNumber %in% duplicate_ids) %>%
  arrange(AniLifeNumber)

animals_meta_dedup <- animals_meta_farm1 %>%
  distinct(AniLifeNumber, .keep_all = TRUE) # dedup dataframe

View(animals_history)


lactation_metrics <- lactation_metrics %>%
  left_join(
    animals_meta_dedup %>%
      select(AniLifeNumber, AniActive, exit_date, exit_code),
    by = "AniLifeNumber"
  )

lactation_metrics <- lactation_metrics %>%
  group_by(AniLifeNumber, RemLactation_LacNumber) %>%
  mutate(
    max_lac_in_inactive_cows = if (!is.na(exit_code)) {
      max(RemLactation_LacNumber[AniActive == FALSE], na.rm = TRUE)
    } else {
      NA_integer_
    },
    last_lactation = if_else(
      !is.na(max_lac_in_inactive_cows) & RemLactation_LacNumber == max_lac_in_inactive_cows,
      TRUE,
      FALSE)
    ) %>%
  ungroup() %>%
  select(-max_lac_in_inactive_cows)

# Calculate the interval between calving and next insemination


# Get first insemination date per lactation
insem_first <- insem_lactation %>%
  group_by(LacAniId, CalculatedLactationCycle) %>%
  summarise(first_insem = min(InsDate), .groups='drop')

# Calculate the interval
insem_first <- insem_first %>%
  left_join(
    lactation_data %>%
      select(LacAniId, LacNumber, LacCalvingDate),
    by = c("LacAniId", "LacNumber")
  ) %>%
  mutate(
    calving_to_insem = interval(LacCalvingDate, first_insem) %/% days(1),
    calving_to_insem = ifelse(calving_to_insem < 20 | calving_to_insem > 200, NA, calving_to_insem)
  )

# Join the first insemination data back to the main dataset
insem_lactation <- insem_lactation %>%
  left_join(
    insem_first %>% select(LacAniId, LacNumber, calving_to_insem),
    by = c("LacAniId", "LacNumber")
  ) %>%
  group_by(LacAniId, LacNumber) %>%
  fill(calving_to_insem, .direction = "downup") %>%
  ungroup()
