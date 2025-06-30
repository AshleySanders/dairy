# ------------------------------------------------------------------------------
# Script Name:     03_calc_lac_interval_dry_off.R
# Author:          Ashley Sanders
# Date Created:    2025-06-26
# Last Modified:   2025-06-26
#
# Description:
# This script calculates lactation dry-off dates, intervals, and key lactation-
# level metrics from Lely milk production data. It identifies lactation cycles
# based on milking gaps > 7 days, calculates cycle-level milk totals and fat/protein
# averages, and joins the results to known calving dates and cow metadata.
#
# Input Data:
# - milk_cows          (daily milk production + animal metadata, joined in 01_save_sql_tables.R)
# - lactation_animal   (calving & reproductive metadata per cow-lactation from RemLactation)
#
# Output Data:
# - lac_calving_calculated.csv: One row per cow-lactation with calculated lactation metrics
#
# Dependencies:
# - Packages: dplyr, lubridate, tidyverse, ggplot2, fuzzyjoin, here
# - Scripts: 01_save_sql_tables.R must be run first
#
# Notes:
# - This script assumes milk_cows and lactation_animal are loaded into memory
# - Dry-off intervals are estimated from milking gaps only
# - Age at first calving is joined post-calculation
# - Still-milking flag is used to interpret missing dry-off dates
# ------------------------------------------------------------------------------


# Load necessary libraries.

library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(fuzzyjoin)

# Step 1: Convert date column to Date type
milk_cows <- milk_cows %>%
  mutate(MdpProductionDate = as.Date(MdpProductionDate))

# Step 2: Arrange by cow and date, then calculate the interval (delta)
milk_cows <- milk_cows %>%
  arrange(AniLifeNumber, MdpProductionDate) %>%
  group_by(AniLifeNumber) %>%
  mutate(
    next_production_date = lead(MdpProductionDate),
    delta_days = as.numeric(next_production_date - MdpProductionDate)
  )

# Step 3: Identify dry-off dates (interval > 7 days or end of record)
milk_cows <- milk_cows %>%
  mutate(
    dry_off_flag = if_else(is.na(delta_days) | delta_days > 7, TRUE, FALSE),
    dry_off_date = if_else(dry_off_flag, MdpProductionDate, as.Date(NA))
  )

# Step 4: Create LacNumber based on the milking production data: For each subsequent lactation cycle identified per AniLifeNumber, we will add a column with LacNumber for each row with milking data that corresponds to the that AniLifeNumber and increase the LacNumber by 1 for each subsequent lactation cycle.

milk_cows <- milk_cows %>%
  mutate(
    lactation_change = if_else(dry_off_flag, 1, 0),
    LacNumber = cumsum(lag(lactation_change, default = 1)) # Start at LacNumber = 1
  )

# Step 5: Assign dry_off_interval
milk_cows <- milk_cows %>%
  mutate(
    dry_off_interval = if_else(dry_off_flag, delta_days, as.numeric(NA))
  )

# Step 6: Add a column for a flag: "still_milking" that should either be true or false based on the following logic: if the last available milking production date for an AniLifeNumber is either 18/09/2024 or 19/09/2024, set the still_milking flag to TRUE for all rows of that lactation cycle. It should be false for all other rows.

milk_cows <- milk_cows %>%
  group_by(AniLifeNumber, LacNumber) %>%
  mutate(
    last_milk_date = max (MdpProductionDate, na.rm = TRUE),
    still_milking = last_milk_date %in% as.Date(c("2024-09-18", "2024-09-19"))
  ) %>%
  ungroup()

# Step 7a: Create a subset of cow - lactation cycle data to add to lactation_animal.

milk_cows <- milk_cows %>%
  rename(
    lac_number_calculated = LacNumber
  )

# Step 7b: Determine whether to use mean or median for summarization
library(ggplot2)

# Visualize fat percentage distribution
ggplot(milk_cows, aes(x = MdpFatPercentage)) +
  geom_histogram(bins = 50) +
  ggtitle("Distribution of Fat %")

# Visualize protein percentage
ggplot(milk_cows, aes(x = MdpProteinPercentage)) +
  geom_histogram(bins = 50) +
  ggtitle("Distribution of Protein %")

# Step 7 c: Create summary table
lactation_calculated <- milk_cows %>%
  group_by(AniLifeNumber, lac_number_calculated) %>%
  summarise(
    AniBirthday = first(AniBirthday),
    AniActive = first(AniActive),
    milk_production_start_date = min(MdpProductionDate, na.rm = TRUE),
    milk_production_end_date = max(MdpProductionDate, na.rm = TRUE),
    last_milk_date = max(MdpProductionDate, na.rm = TRUE),
    total_milk_prod = sum(MdpDayProduction, na.rm = TRUE),
    mean_fat_percent = mean(MdpFatPercentage, na.rm = TRUE),
    mean_protein_percent = mean(MdpProteinPercentage, na.rm = TRUE),
    dry_off_date = if (all(is.na(dry_off_date))) NA_Date_ else max(dry_off_date, na.rm = TRUE),
    dry_off_interval = if (all(is.na(dry_off_interval))) NA_real_ else max(dry_off_interval, na.rm = TRUE),
    still_milking = any(still_milking),
    .groups = "drop"
  ) %>%
  mutate(
    dry_off_date = if_else(dry_off_date == -Inf, NA_Date_, dry_off_date),
    dry_off_interval = if_else(dry_off_interval == -Inf, NA_real_, dry_off_interval)
  )


# --- Join data from lactation_animal: LacCalvingDate & age_at_first_calving ---

# Ensure dates are in Date format
lactation_animal <- lactation_animal %>%
  mutate(LacCalvingDate = as.Date(LacCalvingDate))

lactation_calculcated <- lactation_calculated %>%
  mutate(milk_production_start_date = as.Date(milk_production_start_date))

# # Perform fuzzy join on calving & milk production start dates with filtering by AniLifeNumber separately
calving_joined <- lactation_calculated %>%
  inner_join(lactation_animal %>% select(AniLifeNumber, LacCalvingDate),
             by = "AniLifeNumber", relationship = "many-to-many") %>%
  filter(
    abs(as.numeric(milk_production_start_date - LacCalvingDate)) <= 10
  ) %>%
  group_by(AniLifeNumber, lac_number_calculated) %>%
  slice_min(abs(as.numeric(milk_production_start_date - LacCalvingDate)), with_ties = FALSE) %>%
  ungroup()

# Join age_at_first_cavling (regular left join)
lac_calving_calculated <- calving_joined %>%
  left_join(
    lactation_animal %>% select(AniLifeNumber, age_at_first_calving) %>% distinct(),
    by = "AniLifeNumber"
  )

# lac_calving_calculated is the summary per cow per lactation cycle of the following variables:
colnames(lac_calving_calculated)

# Final Missingness Checks
sum(is.na(lac_calving_calculated$lac_number_calculated))
sum(is.na(lac_calving_calculated$milk_production_start_date))
sum(is.na(lac_calving_calculated$milk_production_end_date))
sum(is.na(lac_calving_calculated$total_milk_prod))
sum(is.na(lac_calving_calculated$mean_fat_percent))
sum(is.na(lac_calving_calculated$mean_protein_percent))
sum(is.na(lac_calving_calculated$dry_off_date))
sum(is.na(lac_calving_calculated$still_milking))
sum(is.na(lac_calving_calculated$LacCalvingDate))
sum(is.na(lac_calving_calculated$age_at_first_calving))

# Check to ensure there are dry-off intervals for all cows who are not still milking or for whom the lactation cycle is not their last
missing_dryoff_interval <- lac_calving_calculated %>%
  filter(still_milking == FALSE, milk_production_end_date != last_milk_date) %>%
  mutate(missing_dryoff_interval = is.na(dry_off_interval)) %>%
  group_by(missing_dryoff_interval, AniActive) %>%
  summarise(count = n(), .groups = "drop")

missing_dryoff_interval

# Save dataframe
write.csv(lac_calving_calculated, here("data", "lac_calving_calculated.csv"))
