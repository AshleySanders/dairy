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

lactation_summary <- read.csv(here("data", "lactation_summary.csv"))

# Calculate summarize lactation cycle data per cow

lactation_summary_by_cow <- lactation_summary %>%
  group_by(AniLifeNumber) %>%
  summarise(
    number_lactations = max(pmax(RemLactation_LacNumber, CalculatedLactationCycle, na.rm = TRUE), na.rm = TRUE),
       .groups = "drop"
  )

lactation_summary_by_cow <- lactation_summary_by_cow %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber))

# Identify cows that appear in lactation_summary_by_cow who should be included in the features table
valid_cows <- unique(lactation_summary_by_cow$AniLifeNumber)

# Calculate average length of dry off period based on full data set, including cycles that occurrred prior to November 2018
lactation_summary_all <- read.csv(here("data", "lactation_summary_all.csv"))

# Convert dry_off_interval to numeric, filter to valid values for the full lactation summary data set
lactation_summary_all <- lactation_summary_all %>%
  mutate(dry_off_interval_num = as.numeric(dry_off_interval)) %>%
  filter(!is.na(dry_off_interval_num)) %>%   # exclude last cycle (no following start date)
  group_by(AniLifeNumber) %>%
  summarise(
      avg_dry_interval = mean(dry_off_interval_num, na.rm = TRUE),
      .groups = "drop")

lactation_summary_all <- lactation_summary_all %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber))

lactation_summary_all <- lactation_summary_all %>%
  group_by(AniLifeNumber) %>%
  mutate(n_intervals = n()) %>%
  ungroup()



# Calculate first calving ages
first_calving <- lactation_animal %>%
  filter(LacNumber == 1, !is.na(AniBirthday), !is.na(LacCalvingDate)) %>%
  group_by(AniLifeNumber) %>%
  slice_min(LacCalvingDate, with_ties = FALSE) %>%
  summarise(
    age_first_calving_months = interval(AniBirthday, LacCalvingDate) %/% months(1),
    .groups = "drop"
  )
first_calving <- first_calving %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber))


# Aggregate daily milk data per cow
cow_milk_summary <- milk_cows %>%
  mutate(
    AniLifeNumber = clean_ani(AniLifeNumber),
    has_history_data = AniLifeNumber %in% animals_meta_farm1$animal
  ) %>%
  group_by(AniLifeNumber, has_history_data) %>%
  summarise(
    avg_daily_milk = mean(MdpDayProduction, na.rm = TRUE) / 1.03, # convert to liters
    milk_span_days = as.numeric(max(MdpProductionDate, na.rm = TRUE) - min(MdpProductionDate, na.rm = TRUE)),
    avg_milkings_per_day = mean(MdpMilkings, na.rm = TRUE),
    fat_pct_avg = mean(MdpFatPercentage, na.rm = TRUE),
    protein_pct_avg = mean(MdpProteinPercentage, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare animal identity and status data (already partially joined in milk_cows) ---
cow_identity <- milk_cows %>%
  select(AniLifeNumber, AniGenId, AniBirthday, AniActive) %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber)) %>%
  distinct()

# Prepare entry & exit data (from animals_history) ---
animals_meta_farm1 <- animals_meta %>%
  filter(customer_id == "16450bc2-f930-4052-a3f7-a602646e64cc",
         race == "66") %>%
  mutate(animal = clean_ani(animal),
         entry_date = as.Date(entry_date),
         exit_date = as.Date(exit_date)) %>%
  select(animal, entry_code, entry_date, exit_code, exit_date) %>%
  distinct(animal, .keep_all = TRUE)


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
  mutate(AniLifeNumber = ani_clean(AniLifeNumber))
  filter(!is.na(AniLifeNumber), AniLifeNumber != "",
         AniLifeNumber %in% valid_cows) %>%
  left_join(cow_identity, by = "AniLifeNumber") %>%
  left_join(animals_meta_farm1, by = c("AniLifeNumber" = "animal")) %>%
  left_join(slaughter_cleaned, by = c("AniLifeNumber" = "national_number")) %>%
  left_join(lactation_summary_by_cow, by = "AniLifeNumber") %>%
  left_join(lactation_summary_all, by = "AniLifeNumber") %>%
  mutate(
    birth_year = year(AniBirthday),
    AniActive = if_else(!is.na(exit_date), FALSE, AniActive),
    exit_date = coalesce(exit_date, slaughter_date),
    exit_code = if_else(is.na(exit_code) & !is.na(slaughter_date), "B", exit_code),
    age_at_exit_days = as.numeric(exit_date - AniBirthday)
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
    birth_year >= 2016 & birth_year <= 2018 ~ "2016–2018",
    birth_year >= 2019 & birth_year <= 2021 ~ "2019–2021",
    birth_year >= 2022 ~ "2022+"
  ))

# Create flag for missing dry interval data
cow_features <- cow_features %>%
  left_join(
    lactation_summary_all %>% select(AniLifeNumber, n_intervals) %>% distinct(),
    by = "AniLifeNumber"
  ) %>%
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

