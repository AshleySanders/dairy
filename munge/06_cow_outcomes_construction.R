# Description: The goal of this script is to create the table with all outcome variables to gauge the profitability of herd management strategies per farm.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily


# --- Load Required Packages ---
library(dplyr)
library(lubridate)
library(stringr)

# ---Data
# Use the milk_cows table already created in munge/01_save_sql_tables.R. This table is a join between MilkDayProduction table from Lely and HemAnimal table from Lely.
# Use the full_milk_by_cow table, which calculates the monthly milk production that is conserved for each cow.

# Load the cow_features table created in munge/02_cow_features_construction.R
cow_features <- readRDS(here::here("data", "cow_features.rds"))


# Pull in information that comes from the cow_features table
cow_outcomes <- cow_features %>%
  select(AniLifeNumber, avg_daily_milk_liters, avg_lactation_duration, birth_year)

# Add a high_yield_flag based on the top 25% of avg_daily_milk
cow_outcomes <- cow_outcomes %>%
  mutate(top_25_threshold <- quantile(cow_outcomes$avg_daily_milk, 0.75, na.rm = TRUE))

# Add a short span flag based on milk_span_days
cow_outcomes <- cow_outcomes %>%
  mutate(short_span_flag = avg_lactation_duration < 305)


# Calculate the lifetime total milk and average monthly milk produced (in liters) per cow based on conserved milk (Oct 2020-Aug 2024 and estimates for kept milk prior to Oct 2020)
milk_prod_summary_by_cow <- full_milk_by_cow %>%
  group_by(AniLifeNumber) %>%
  summarise(
    total_milk_liters = sum(MonthlyMilkYield_Liters, na.rm = TRUE),
    avg_monthly_milk = mean(MonthlyMilkYield_Liters, na.rm = TRUE),
    .groups = "drop"
  )


# Calculate prod_decline_90d as the drop in milk between first 30 days and following 60 days



# Join everything into cow_outcomes

cow_outcomes <- cow_outcomes %>%
  left_join(milk_prod_summary_by_cow, by = "AniLifeNumber") %>%
  left_join(animals_slaughter_farm1, by = c("AniLifeNumber" = "national_number"))



# Save or View the result ---
saveRDS(cow_outcomes, file = here::here("data", "cow_outcomes.rds"))

View(cow_outcomes)