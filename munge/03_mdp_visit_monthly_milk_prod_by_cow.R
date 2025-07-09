# ------------------------------------------------------------------------------
# Script Name:    lely_milk_comparison.R
# Project:        Cockpit Agriculture – Lely Milk Production Analysis
# Purpose:        Compare monthly per-cow milk production values between
#                 MilkVisit × DeviceVisit (filtered for conserved milk)
#                 and MilkDayProduction (all milk) to understand discrepancies.
#
# Description:    This script reads in two pre-processed datasets from SQL,
#                 cleans and formats the data, joins the datasets by cow and
#                 month, calculates the difference in reported milk volumes,
#                 and visualizes the magnitude and direction of discrepancies.
#                 It then filters out partial-month data and combines actual
#                 conserved milk volumes with estimates from MDP (pre-Nov 2020)
#                 to create a unified monthly cow-level dataset.
#
# Inputs:
#   - data/mdp_monthly_milk_production_by_cow.csv
#   - data/monthly_saved_prod_by_cow_2020Oct_2024Aug.csv
#   - data/mdp_monthly_kept_milk_estimates.csv
#
# Outputs:
#   - full_milk_by_cow (bound table of estimated + measured conserved milk)
#   - Summary statistics and plots of production differences by month
#
# Author:         Ashley Sanders
# Last updated:   2025-07-08
# ------------------------------------------------------------------------------



# Lely milk production reporting
# Compare the monthly milk production per cow between MilkVisit x DeviceVisit and MilkDayProduction to determine if the latter counts all milk or only conserved milk.

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
