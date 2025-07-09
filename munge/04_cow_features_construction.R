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
lactation_summary <- read.csv(here("data", "lactation_summary.csv"))

# Aggregate daily milk data per cow
cow_milk_summary <- milk_cows %>%
  mutate(
    AniLifeNumber = str_replace_all(str_trim(as.character(AniLifeNumber)), " ", ""),
    has_history_data = AniLifeNumber %in% animals_history$animal
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
  mutate(AniLifeNumber = str_replace_all(str_trim(as.character(AniLifeNumber)), " ", "")) %>%
  distinct()

# Prepare entry & exit data (from animals_history) ---
animals_history_cleaned <- animals_history %>%
  filter(customer_id == "16450bc2-f930-4052-a3f7-a602646e64cc",
         race == "66") %>%
  mutate(animal = str_trim(as.character(animal))) %>%
  select(animal, entry_code, entry_date, exit_code, exit_date) %>%
  distinct(animal, .keep_all = TRUE)

# Prepare slaughter data (optional additional outcome) ---
animals_slaughter <- dbGetQuery(prod, "
  SELECT
    national_number,
    date,
    weight
  FROM animals_slaughter
  ORDER BY national_number, date
                                ")

slaughter_cleaned <- animals_slaughter %>%
  mutate(
    national_number = str_trim(as.character(national_number)),
    slaughter_date = date  # rename
  ) %>%
  select(national_number, slaughter_date, weight) %>%
  distinct(national_number, .keep_all = TRUE)

# Join everything into cow_features ---
cow_features <- cow_milk_summary %>%
  filter(!is.na(AniLifeNumber), AniLifeNumber != "") %>%
  left_join(cow_identity, by = "AniLifeNumber") %>%
  left_join(animals_history_cleaned, by = c("AniLifeNumber" = "animal")) %>%
  left_join(slaughter_cleaned, by = c("AniLifeNumber" = "national_number")) %>%
  left_join(
    lactation_animal %>% select(AniLifeNumber, age_at_first_calving) %>% distinct(),
    by = "AniLifeNumber"
  ) %>%
  mutate(
    birth_year = year(AniBirthday),
    AniActive = if_else(!is.na(exit_date), FALSE, AniActive),
    exit_date = coalesce(exit_date, slaughter_date),
    exit_code = if_else(is.na(exit_code) & !is.na(slaughter_date), "B", exit_code),
    age_at_exit_days = as.numeric(exit_date - AniBirthday)
  )

# Join age_at_first_calving (regular left join)
lac_calving_calculated <- calving_joined %>%


# Create cohorts
cow_features <- cow_features %>%
  mutate(cohort = case_when(
    birth_year < 2016 ~ "pre-2016",
    birth_year >= 2016 & birth_year <= 2018 ~ "2016-2018",
    birth_year > 2018 ~ "post-2019"
  ))


# Save or View the result ---
saveRDS(cow_features, file = here::here("data", "cow_features.rds"))

View(cow_features)

