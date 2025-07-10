# Calculate measures by lactation cycle:
# - age_at_calving
# - Total number of artificial insemination attempts
# - Number of failed artificial insemination attempts
# - Ratio: successful / total insemination attempts
# - Number of failed pregnancies
# - Total Milk Yield (liters)
# - avg_daily_yield
# - early_lactation_yield : first 30 days
# - mid_lactation_yield : subsequent 60 days
# - delta_early_mid_yield : Difference between the first 30 days to next 60 days of milk production (liters)
# - Avg percent fat
# - Avg percent protein
# - is_final_cycle : Flag for last lactation cycle (?)
# - calving_to_first_insem_days
# - endmilk_to_exit_days

# Consider calculating: days_to_peak_yield


# Other variables to keep for correlation analysis
# - lactation_duration
# - dry_off_interval
# - LacCalvingDate
# - milk_production_start_date
# - milk_production_end_date
# - LacColostrumDate

# Identifiers to keep
# - AniId
# - AniLifeNumber
# - InsId
# - LacId
# - CalculatedLactationCycle
# - RemLactation_LacNumber

# Variables to consider joining from other data sets
# - exit_date
# - exit_code (exit information for correlation analysis with failed inseminations and failed pregnancies)
# - slaughter date (for comparison with last milk_production_end_date to determine if the farmer fattened the cow before the sale to the Abbatoir => add a flag for "fattened" so we can compare profitability of this strategy)

# NOTE: The insem_lac_preg dataset does NOT show multiple insemination attempts that never resulted in a pregnancy. This information should also be examined to create a complete picture for each cow, beyond lactation cycles

# Data :
# -

# Load libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

# Load the cow_features table created in munge/02_cow_features_construction.R
insem_lac_preg <- readRDS(here::here("data", "lac_insem_preg.rds"))
