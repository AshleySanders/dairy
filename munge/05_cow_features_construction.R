# Description: The goal of this script is to create the table with all feature variables to analyze herd management strategies per farm.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily

# ---Data
# lactation_metrics
# cow_health_summary
# dairy_meta_farm_1

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")


# Compute age_at_exit
exit_age <- lactation_metrics %>%
  mutate(
    AniBirthday = as.Date(AniBirthday),
    exit_date = as.Date(exit_date),
    age_at_exit = if_else(
    !is.na(exit_date),
    interval(AniBirthday, exit_date) %/% months(1),
    NA_integer_
  ))

# Compute number of days between last milking date and exit date
endmilk_to_exit <- lactation_metrics %>%
  group_by(AniLifeNumber) %>%
  mutate(
    endmilk_to_exit_days = if_else(
      last_lactation == TRUE & !is.na(exit_date),
      as.numeric(as.Date(exit_date) - as.Date(milk_production_end_date)),
      NA_real_
    )
  ) %>%
  ungroup()

cow_lactation_summary <- lactation_metrics %>%
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
    number_lactations         = max(pmax(RemLactation_LacNumber, CalculatedLactationCycle, na.rm = TRUE), na.rm = TRUE),
    avg_lactation_duration    = mean(lactation_duration, na.rm = TRUE),
    avg_dry_interval          = mean(dry_off_interval, na.rm = TRUE),
    n_intervals               = sum(!is.na(dry_off_interval)),
    age_at_first_calving      = min(age_at_calving, na.rm = TRUE),
    latest_lactation_date     = max(milk_production_start_date, na.rm = TRUE),

    # animal descriptors (will be recycled per group)
    entry_code                = first(entry_code),
    entry_date                = first(entry_date),
    exit_code                 = first(exit_code),
    exit_date                 = first(exit_date),
    AniBirthday               = first(AniBirthday),
    AniGenId                  = first(AniGenId),

    .groups = "drop"
  )


# Prepare animal identity and status data (already partially joined in milk_cows) ---
cow_identity <- dairy_meta_farm1 %>%
  select(national_number, race, slaughter_date, weight, selling_price) %>%
  mutate(national_number = clean_ani(national_number)) %>%
  distinct()


# Join everything into cow_features ---
cow_features <- cow_identity %>%





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

