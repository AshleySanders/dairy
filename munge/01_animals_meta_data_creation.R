# ------------------------------------------------------------------------------
# Script Name:    01_animals_meta_data_creation.R
# Project:        Cockpit Agriculture – Herd Management Strategy
# Purpose:        Create clean animal-level metadata for a specific farm from
#                 Supabase tables. Includes herd history, demographics, and exit
#                 info for all dairy cows.
#
# Description:    This script:
#                 - Extracts and cleans animal and herd history data
#                 - Derives entry/exit metadata and age-at-exit
#                 - Filters for dairy cows (excludes males)
#                 - Joins slaughter data to supplement missing exit info
#                 - Cleans national numbers and dates
#                 - Checks for join mismatches with Lely HemAnimal table
#
# Inputs:
#   - Supabase tables: animals, animals_history, animals_slaughter
#   - Lely HemAnimal table (for join validation)
#
# Outputs:
#   - cache/dairy_meta_farm1.RData: Cleaned metadata for all dairy cows
#
# Notes:
#   - This script is farm-specific and uses hardcoded customer_id
#   - Manual corrections should be handled via farm-specific script in /lib
#   - Re-cache HemAnimal after running any corrections
#
# Author:         Ashley Sanders
# Created:        2025-07-18
# Last updated:   2025-08-20
# ------------------------------------------------------------------------------

# Load config and helpers
source(here::here("config", "farm5_config.R"))
source(here::here("lib", "helpers.R"))


# Clean and filter animals_history
assign(paste0(farm_prefix, "_animals_history"),
       animals_history %>%
         filter(customer_id == farm5_customer_id) %>%
         filter(date < as.Date("2025-03-12")) %>% # Change for each farm
         distinct() %>%
         mutate(
           national_number = clean_ani(animal),
           date = as.Date(date),
           entry_date = as.Date(entry_date),
           exit_date = as.Date(exit_date)
         ) %>%
         select(-animal)
)


# Import and clean slaughter data
assign(paste0(farm_prefix, "_animals_slaughter"),
       animals_slaughter %>%
         filter(customer_id == farm5_customer_id) %>%
         distinct() %>%
         mutate(AniLifeNumber = clean_ani(national_number),
                slaughter_date = as.Date(date),
                classification = dplyr::recode(classification,
                                               "O=3" = "O-3", "P+2" = "P-2",
                                               "P+3" = "P-3", "P+4" = "P-4",
                                               "P=2" = "P-2", "P=3" = "P-3"
                )) %>%
         select(-c(created_at, customer_id, date))
)



# Filter the metadata to dairy cows
assign(paste0(farm_prefix, "_dairy_history"),
       get(paste0(farm_prefix, "_animals_history")) %>%
         mutate(category = str_trim(category)) %>%
         filter(category != "MA") %>%
         arrange(national_number, desc(date)) %>%
         group_by(national_number) %>%
         slice(1) %>%
         ungroup()
)

# ! Run any necessary manual correction from /lib

cache(paste0(farm_prefix, "_animals_history"))

# Check the quality of animal national numbers and metadata in Lely
assign(paste0(farm_prefix, "_HemAnimal"),
       get(paste0(farm_prefix, "_HemAnimal")) %>%
         mutate(AniLifeNumber = clean_ani(AniLifeNumber),
                AniBirthday = as.Date(AniBirthday),
                AniMotherLifeNumber = clean_ani(AniMotherLifeNumber)) %>%
         distinct()
)


# Capture all national numbers of cows in the herd
herd_ids <- unique(get(paste0(farm_prefix, "_HemAnimal"))$AniLifeNumber)

# Join HemAnimal with dairy history and add entry codes & dates
fm5_dairy_meta <- fm5_dairy_history %>%
  left_join(fm5_HemAnimal, by = c("national_number" = "AniLifeNumber")) %>%
  mutate(
    entry_code = if_else(is.na(entry_code) & AniMotherLifeNumber %in% herd_ids, "N", entry_code),
    entry_date = if_else(!is.na(entry_date), entry_date,
                         if_else(entry_code == "N", AniBirthday, as.Date(NA)))
  )


# Quick checks
meta <- get(paste0(farm_prefix, "_dairy_meta"))
table(meta$entry_code)
cat("Missing entry_code:", sum(is.na(meta$entry_code)), "\n")
cat("Missing entry_date:", sum(is.na(meta$entry_date)), "\n")
cat("Missing exit_code:", sum(is.na(meta$exit_code)), "\n")
cat("Missing exit_date:", sum(is.na(meta$exit_date)), "\n")


cache(paste0(farm_prefix, "_dairy_meta"))

# Create a subset of animal_health data for dairy cows
animal_health <- animal_health %>% mutate(AniLifeNumber = clean_ani(AniLifeNumber))
assign(paste0(farm_prefix, "_dairy_health"),
       animal_health %>% filter(AniLifeNumber %in% herd_ids)
)
cache(paste0(farm_prefix, "_dairy_health"))
