# ------------------------------------------------------------------------------
# Script Name:    lely_milk_comparison.R
# Project:        Cockpit Agriculture – Lely Milk Production Analysis
#
# Purpose:        Quantify the difference between total and conserved milk
#                 reported by Lely across two systems:
#                 - MilkVisit × DeviceVisit (conserved milk only)
#                 - MilkDayProduction (includes all milk, incl. discarded)
#
# Description:    This script reads in cow-level monthly milk production from
#                 both systems. It:
#                   • Aligns time periods and formats
#                   • Calculates the monthly difference in liters per cow
#                   • Visualizes systematic differences
#                   • Filters out partial-month data (Oct 2020, Sept 2024)
#                   • Combines measured conserved milk (Nov 2020–Aug 2024)
#                     with estimated conserved milk (pre-Nov 2020)
#                   • Produces a clean, unified dataset of monthly milk
#                     production per cow for modeling and aggregation
#
# Inputs:
#   - data/mdp_monthly_milk_production_by_cow.csv
#   - data/monthly_saved_prod_by_cow_2020Oct_2024Aug.csv
#   - data/mdp_monthly_kept_milk_estimates.csv
#   - data/milk_production_monthly_farm1.csv
#
# Outputs:
#   - full_milk_by_cow.csv: unified monthly dataset of conserved milk by cow
#   - full_milk_monthly: total monthly milk yield across all cows
#   - Summary plot: MDP vs Visit-based differences
#
# Author:         Ashley Sanders
# Last updated:   2025-07-09
# ------------------------------------------------------------------------------

# Load necessary libraries

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# The following two datasets are the results of SQL queries in VSC

mdp <- read.csv(here("data", "mdp_monthly_milk_production_by_cow.csv"))
visit <- read.csv(here("data", "monthly_saved_prod_by_cow_2020Oct_2024Aug.csv"))

visit <- visit %>%
  filter(Month != "NULL") # Remove null months

# Convert the month column into date data types
mdp <- mdp %>%
  mutate(Month = ymd(paste0(Month, "-01")))

visit <- visit %>%
  mutate(Month = ymd(paste0(Month, "-01")))

min_month = min(visit$Month)

mdp_filtered <- mdp %>%
  filter(Month >= min_month)

monthly_milk_diff <- visit %>%
  left_join(mdp_filtered, by = c("AniLifeNumber", "Month"))

colnames(monthly_milk_diff)

monthly_milk_diff <- monthly_milk_diff %>%
  mutate(
    milk_diff_liters = MonthlyMilkYield_Liters - mdpMonthlyMilkYield_Liters)

summary(monthly_milk_diff$milk_diff_liters)

# Ensure that the Month data type is read as a date
monthly_milk_diff <- monthly_milk_diff %>%
  mutate(Month = as.Date(Month))

# Plot the differences
ggplot(monthly_milk_diff, aes(x = Month, y = milk_diff_liters)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Monthly Difference in Milk Production by Cow: MDP vs Vist*Device",
    subtitle = "Positive = More milk in Visit data; Negative = More milk in MDP",
    x = "Month",
    y = "Difference in Milk Yield (Liters)") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Remove the data from October 2020 since it looks like we don't have data from all dates in that month, and remove September 2024 since the backup was made on September 19, and we don't have a full month's worth of data.

visit <- visit %>%
  filter(Month >= as.Date("2020-11-01") & Month < as.Date("2024-09-01"))

# Load the estimates from MDP
mdp_estimates <- read.csv(here("data", "mdp_monthly_kept_milk_estimates.csv"))

# Ensure that Month is read as a date - the last day of the month that the milk was collected for that invoice period.
mdp_estimates <- mdp_estimates %>%
  filter(!is.na(Month), Month != "NULL", Month != "") %>%
  mutate(Month = paste0(Month, "-01"),
         Month = ymd(Month),
         Month = ceiling_date(Month, "month") - days(1))

visit <- visit %>%
  mutate(Month = ceiling_date(Month, "month") - days(1))

# Examine the column names and data types for each table
colnames(mdp_estimates)
colnames(visit)

glimpse(mdp_estimates)
glimpse(visit)

# Rename the mismatched column in mdp_estimates
mdp_estimates <- mdp_estimates %>%
  rename(MonthlyMilkYield_Liters = mdpMonthlyMilkYield_Liters)

# Create full monthly milk production dataset by cow with the actual values of conserved milk (November 2020 - August 2024) and the estimates prior to November 2020.
full_milk_by_cow <- bind_rows(mdp_estimates, visit)

View(full_milk_by_cow)

write.csv(full_milk_by_cow, here("data", "full_monthly_milk_prod_by_cow.csv"))

# Summarize estimated conserved milk produced by month
full_milk_monthly <- full_milk_by_cow %>%
  group_by(Month) %>%
  summarise(sum(MonthlyMilkYield_Liters), .groups = "drop")

View(full_milk_monthly)

# The complete data set for quantities of milk produced and delivered monthly, along with monthly revenues, the difference between Lely milking data and Invoice quantities and the ratio of that difference are all available in
# milk_production_monthly_farm1.csv

milk_production_monthly_farm1 <- read.csv(here("data", "milk_production_monthly_farm1.csv"))