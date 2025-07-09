# Description: The goal of this script is to create the table with all outcome variables to gauge the profitability of herd management strategies per farm.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily


# --- Load Required Packages ---
library(dplyr)
library(lubridate)
library(stringr)

# ---Data
# Use the milk_cows table already created in munge/01_save_sql_tables.R. This table is a join between MilkDayProduction table from Lely and HemAnimal table from Lely.

# Load the cow_features table created in munge/02_cow_features_construction.R
cow_features <- readRDS(here::here("data", "cow_features.rds"))


# Pull in information that comes from the cow_features table
cow_outcomes <- cow_features %>%
  select(AniLifeNumber, avg_daily_milk, milk_span_days, birth_year)

# Add a high_yield_flag based on the top 25% of avg_daily_milk
cow_outcomes <- cow_outcomes %>%
  mutate(top_25_threshold <- quantile(cow_outcomes$avg_daily_milk, 0.75, na.rm = TRUE))

# Add a short span flag based on milk_span_days
cow_outcomes <- cow_outcomes %>%
  mutate(short_span_flag = milk_span_days < 305)

cow_outcomes <- cow_outcomes %>%
  mutate(cohort = case_when(
    birth_year < 2016 ~ "pre-2016",
    birth_year >= 2016 & birth_year <= 2018 ~ "2016-2018",
    birth_year > 2018 ~ "post-2019"
  ))

# Calculate prod_decline_90d as the drop in milk between first 30 days and following 60 days
# ---- NOTE: Waiting on Mil'Klic data.


# Save or View the result ---
saveRDS(cow_outcomes, file = here::here("data", "cow_outcomes.rds"))

View(cow_outcomes)