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
# Load config and helpers
source(here::here("config", "farm1_config.R"))
source(here::here("lib", "helpers.R"))

# Filter out routine dry-off and vaccination events
assign(paste0(farm_prefix, "_dairy_health_problems"),
       get(paste0(farm_prefix, "_dairy_health")) %>%
         filter(
           DisName != "Drying-off",
           !str_detect(tolower(DisName), "vaccin")
         )
)


cache(paste0(farm_prefix, "_dairy_health_problems"))

# Standardize IDs and convert date
assign(paste0(farm_prefix, "_dairy_health_problems"),
       get(paste0(farm_prefix, "_dairy_health_problems")) %>%
         mutate(
           AniLifeNumber = clean_ani(AniLifeNumber),
           DiaDate = as.Date(DiaDate)
         )
)

# Aggregate per-cow health metrics
# - n_health_problems: total count of events
# - recovery_duration: sum of cure periods (in days)
# - date_last_diagnosis: most recent event date
# - last_diagnosis: name of health issue at that date
# These will be joined later into cow_features for modeling

# Aggregate health metrics per cow
assign(paste0(farm_prefix, "_cow_health_summary"),
       get(paste0(farm_prefix, "_dairy_health_problems")) %>%
         group_by(AniLifeNumber) %>%
         summarise(
           n_health_problems = n(),
           recovery_duration = sum(DisCurePeriod, na.rm = TRUE),
           date_last_diagnosis = max(DiaDate, na.rm = TRUE),
           last_diagnosis = DisName[which.max(DiaDate)],
           .groups = "drop"
         )
)


cache(paste0(farm_prefix, "_cow_health_summary"))
write.csv(get(paste0(farm_prefix, "_dairy_health_problems")), here("data", paste0(farm_prefix, "_dairy_health_problems.csv")))