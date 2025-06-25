# Description: The goal of this script is to create the table with all feature variables to analyze herd management strategies per farm.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily


# --- Load Required Packages ---
library(dplyr)
library(lubridate)
library(stringr)

# ---Data
# Use the milk_cows table already created in munge/01_save_sql_tables.R. This table is a join between MilkDayProduction table from Lely and HemAnimal table from Lely.

# Aggregate daily milk data per cow
cow_milk_summary <- milk_cows %>%
  mutate(
    AniLifeNumber = str_replace_all(str_trim(as.character(AniLifeNumber)), " ", ""),
    has_history_data = AniLifeNumber %in% animals_history$animal
  ) %>%
  group_by(AniLifeNumber, has_history_data) %>%
  summarise(
    total_milk = sum(MdpDayProduction, na.rm = TRUE),
    avg_daily_milk = mean(MdpDayProduction, na.rm = TRUE),
    milk_span_days = as.numeric(max(MdpProductionDate, na.rm = TRUE) - min(MdpProductionDate, na.rm = TRUE)),
    avg_milkings_per_day = mean(MdpMilkings, na.rm = TRUE),
    fat_pct_avg = mean(MdpFatPercentage, na.rm = TRUE),
    protein_pct_avg = mean(MdpProteinPercentage, na.rm = TRUE),
    .groups = "drop"
  )

# Add year-month column to milk data
monthly_production <- milk_cows %>%
  mutate(
    AniLifeNumber = str_replace_all(str_trim(as.character(AniLifeNumber)), " ", ""),
    year_month = format(as.Date(MdpProductionDate), "%Y-%m")
  ) %>%
  group_by(AniLifeNumber, year_month) %>%
  summarise(
    monthly_total = sum(MdpDayProduction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(AniLifeNumber) %>%
  summarise(
    avg_monthly_milk = mean(monthly_total, na.rm = TRUE),
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
  filter(!is.na(AniLifeNumber), AniLifeNumber != "") %>% # Remove empty strings too
  left_join(monthly_production, by = "AniLifeNumber") %>%
  left_join(cow_identity, by = "AniLifeNumber") %>%
  left_join(animals_history_cleaned, by = c("AniLifeNumber" = "animal")) %>%
  left_join(slaughter_cleaned, by = c("AniLifeNumber" = "national_number"))

# Optional final cleanup ---
cow_features <- cow_features %>%
  mutate(
    birth_year = year(AniBirthday),
    # Update active status based on exit_date
    AniActive = if_else(!is.na(exit_date), FALSE, AniActive),
    exit_date = if_else(is.na(exit_date) & !is.na(slaughter_date), slaughter_date, exit_date),
    exit_code = if_else(is.na(exit_code) & !is.na(slaughter_date), "B", exit_code)
    )

# Save or View the result ---
saveRDS(cow_features, file = here::here("data", "cow_features.rds"))

View(cow_features)

