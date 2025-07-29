# -----------------------------------------------------------------------------
# Script Name: 04_dairy_health_measures.R
# Author: Ashley Sanders
# Date Created: 2025-07-23
# Last Updated: 2025-07-29
# Project: Herd Management Strategy Analysis – Dairy Cow Health Features
#
# Description:
# Aggregates raw dairy health event records into per-cow summary variables.
# Filters out routine and preventive entries (dry-offs, vaccinations) to focus on
# actual health problems. Outputs are cached for later joining into cow_features.
#
# Inputs:
# - dairy_health (cached data frame with one row per health event for each cow)
# - Helper function clean_ani() to standardize AniLifeNumber formatting
#
# Outputs:
# - dairy_health_problems: filtered health events (cached)
# - cow_health_summary: per-cow aggregated health metrics
#
# Dependencies:
# - dplyr, stringr, lubridate (via tidyverse)
# - cache() function from ProjectTemplate or similar
# -----------------------------------------------------------------------------

# Load libraries (assumes tidyverse and cache already available)
library(dplyr)
library(stringr)

# Filter out routine dry-off and preventive vaccination records
# to retain only true health problems

dairy_health_problems <- dairy_health %>%
  filter(
    DisName != "Drying-off",                        # remove planned dry-off events
    !str_detect(tolower(DisName), "vaccin")         # remove any vaccination entries
  )

# Cache the filtered health problems dataset for reuse
cache("dairy_health_problems")

# Standardize identifier and parse date column
# DiaDate arrives as character; convert to Date

dairy_health_problems <- dairy_health_problems %>%
  mutate(
    AniLifeNumber    = clean_ani(AniLifeNumber),      # remove spaces/padding
    DiaDate          = as.Date(DiaDate)               # convert to Date type
  )

# Aggregate per-cow health metrics
# - n_health_problems: total count of events
# - recovery_duration: sum of cure periods (in days)
# - date_last_diagnosis: most recent event date
# - last_diagnosis: name of health issue at that date
# These will be joined later into cow_features for modeling

cow_health_summary <- dairy_health_problems %>%
  group_by(AniLifeNumber) %>%
  summarise(
    n_health_problems    = n(),                                          # total health events
    recovery_duration    = sum(DisCurePeriod, na.rm = TRUE),             # total days under treatment
    date_last_diagnosis  = max(DiaDate, na.rm = TRUE),                   # latest event date
    last_diagnosis       = DisName[which.max(DiaDate)],                 # issue corresponding to latest date
    .groups              = "drop"
  )

# Cache final summary table for joining
cache("cow_health_summary")
