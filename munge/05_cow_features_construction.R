# Description: The goal of this script is to create the table with all feature variables to analyze herd management strategies per farm.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily

# ---Data
# lactation_metrics
# cow_health_summary
# dairy_meta_farm1
# animals_slaughter_farm1

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

valid_cow_ids <- unique(lactation_metrics$AniLifeNumber)

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


# Join everything into cow_features ---
cow_features <- cow_lactation_summary %>%
  left_join(dairy_meta_farm1 %>%
              select(-c(AniId, AniGenId, AniBirthday, AniActive, AniMotherLifeNumber, date, customer_id)), by = "AniLifeNumber") %>%
  left_join(exit_age, by = "AniLifeNumber") %>%
  left_join(endmilk_to_exit, by = "AniLifeNumber") %>%
  left_join(cow_insem_summary, by = "AniLifeNumber") %>%
  left_join(cow_health_summary, by = "AniLifeNumber") %>%
  left_join(animals_slaughter_farm1, by = c("AniLifeNumber" = "national_number"))

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

