# Description: The goal of this script is to create the table with all feature variables to analyze herd management strategies per farm.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily


# --- Load Required Packages ---
library(dplyr)
library(lubridate)
library(stringr)

# ---Data
# Use the milk_cows table already created in munge/01_save_sql_tables.R. This table is a join between MilkDayProduction table from Lely and HemAnimal table from Lely.

# Load lactation summary data generated in SQL
# This summary only includes milking cycles that began after 2018-11-01 so that the invoice data matches and we have the full lactation cycle data for each cow for each cycle listed.

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

# --- Calculate summarize lactation cycle data per cow ---

# Calculate average length of dry off period based on full data set, including cycles that occurrred prior to November 2018
lactation_summary_all <- read.csv(here("data", "lactation_summary_all.csv"))

lactation_summary_all <- lactation_summary_all %>%
  rename(AniId = CowID)

# Convert dry_off_interval to numeric, ensure dates are read as Data types, and clean the AniLifeNumber

lactation_summary_all <- lactation_summary_all %>%
  mutate(dry_off_interval_num = as.numeric(dry_off_interval)) %>%
  filter(!is.na(dry_off_interval_num)) %>%   # exclude last cycle (no following start date)
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         milk_production_end_date = as.Date(milk_production_end_date),
         milk_production_start_date = as.Date(milk_production_start_date),
         lactation_duration = as.numeric(milk_production_end_date - milk_production_start_date))

lactation_intervals_per_cow <- lactation_summary_all %>%
  group_by(AniLifeNumber) %>%
  summarise(
    avg_dry_interval = mean(dry_off_interval_num, na.rm = TRUE),
    n_intervals = n(),
    number_lactations = max(pmax(RemLactation_LacNumber, CalculatedLactationCycle, na.rm = TRUE), na.rm = TRUE),
    avg_lactation_duration = mean(lactation_duration, na.rm = TRUE),
    still_milking = any(still_milking == TRUE),
    .groups = "drop"
  )


# Create a dataframe that provides a summary per cow with the latest lactation date
unique_cow_summary <- insem_lac_preg %>%
  group_by(AniLifeNumber) %>%
  summarise(
    AniBirthday = first(AniBirthday),
    number_lactations = max(number_lactations, na.rm = TRUE),
    age_at_first_calving = first(age_at_first_calving),
    latest_lactation_date = max(LacCalvingDate, na.rm = TRUE),
    avg_inseminations = mean(!is.na(InsDate), na.rm = TRUE),
    avg_lactation_length_days = mean(lactation_length_days, na.rm = TRUE),
    avg_calving_to_insem_days = mean(calving_to_insem, na.rm = TRUE),
    avg_next_lactation_dry_days = mean(next_lactation_dry_days, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate daily milk data per cow
cow_milk_summary <- milk_cows %>%
  mutate(
    AniLifeNumber = clean_ani(AniLifeNumber),
    has_history_data = AniLifeNumber %in% animals_meta_farm1$animal
  ) %>%
  group_by(AniLifeNumber, has_history_data) %>%
  summarise(
    avg_daily_milk_liters = mean(MdpDayProduction, na.rm = TRUE) / 1.03, # convert to liters
    avg_milkings_per_day = mean(MdpMilkings, na.rm = TRUE),
    avg_fat_pct = mean(MdpFatPercentage, na.rm = TRUE),
    avg_protein_pct = mean(MdpProteinPercentage, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare animal identity and status data (already partially joined in milk_cows) ---
cow_identity <- milk_cows %>%
  select(AniLifeNumber, AniGenId, AniBirthday, AniActive) %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber)) %>%
  distinct()

# Entry & exit data (dairy_meta_farm1)



# Prepare slaughter data (optional additional outcome) using animals_slaughter table from 01_save_sql_tables.R.

slaughter_cleaned <- animals_slaughter %>%
  mutate(
    national_number = clean_ani(national_number),
    slaughter_date = date,  # rename
    slaughter_date = as.Date(slaughter_date)
  ) %>%
  select(national_number, slaughter_date, weight) %>%
  distinct(national_number, .keep_all = TRUE)

# Join everything into cow_features ---
cow_features <- cow_milk_summary %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber)) %>%
  filter(!is.na(AniLifeNumber), AniLifeNumber != "",
         AniLifeNumber %in% valid_cows) %>%
  left_join(cow_identity, by = "AniLifeNumber") %>%
  left_join(animals_meta_farm1, by = c("AniLifeNumber" = "animal")) %>%
  left_join(slaughter_cleaned, by = c("AniLifeNumber" = "national_number")) %>%
  left_join(lactation_summary_by_cow_post2018, by = "AniLifeNumber") %>%
  left_join(lactation_intervals_per_cow, by = "AniLifeNumber") %>%
  mutate(
    birth_year = year(AniBirthday),
    floor(time_length(interval(AniBirthday, exit_date), "months"))
  )


cow_features <- cow_features %>%
  left_join(
    first_calving %>% select(AniLifeNumber, age_first_calving_months),
    by = "AniLifeNumber"
  )

# Create cohorts
cow_features <- cow_features %>%
  mutate(cohort = case_when(
    birth_year < 2016 ~ "pre-2016",
    birth_year >= 2016 & birth_year <= 2018 ~ "2016-2018",
    birth_year >= 2019 & birth_year <= 2021 ~ "2019-2021",
    birth_year >= 2022 ~ "2022+"
  ))

# Create flag for missing dry interval data
cow_features <- cow_features %>%
  mutate(
    dry_interval_missing_reason = case_when(
      is.na(avg_dry_interval) & n_intervals == 1 ~ "Only 1 lactation",
      is.na(avg_dry_interval) & is.na(n_intervals) ~ "No valid lactation data",
      is.na(avg_dry_interval) ~ "Unknown / Join issue",
      TRUE ~ "OK"
    )
  )

# Save or View the result ---
saveRDS(cow_features, file = here::here("data", "cow_features.rds"))

View(cow_features)

