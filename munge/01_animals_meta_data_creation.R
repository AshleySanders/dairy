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
# Last updated:   2025-07-21
# ------------------------------------------------------------------------------


# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

# Define farm id

farm_id <- "16450bc2-f930-4052-a3f7-a602646e64cc"

# Examining scraped data in supabase to ensure accuracy.

animals_history_farm1 <- animals_history %>%
  filter(customer_id == farm_id)

animals_history_farm1 <-animals_history_farm1 %>%
  mutate(date = as.Date(date))

animals_history_farm1 <- animals_history_farm1 %>%
  filter(date < as.Date("2024-09-19")) # limit this dataframe to match the data coming from the last Lely back-up

animals_history_farm1 <- animals_history_farm1 %>%
  distinct() %>%
  mutate(national_number = clean_ani(animal)) %>%
  select(-animal)

animals_farm1 <- animals %>%
  filter(customer_id == farm_id)

animals_farm1 <- animals_farm1 %>%
  mutate(national_number = clean_ani(national_number)) %>%
  distinct() %>%
  select(national_number, race, customer_id, birth_date, mother, country_code)

animals_meta_farm1 <- animals_history_farm1 %>%
  select(national_number, category, customer_id, date, entry_code, entry_date, exit_code, exit_date)

animals_meta_farm1 <- animals_meta_farm1 %>%
  left_join(animals_farm1 %>%
              select(-customer_id),
            by = "national_number")

# Capture all national numbers of cows in the herd
herd_ids <- unique(animals_meta_farm1$national_number)

# Add an "N" entry_code for cows whose mothers were also in the herd
animals_meta_farm1 <- animals_meta_farm1 %>%
  mutate(
    entry_code = if_else(
      is.na(entry_code) & mother %in% herd_ids,
      "N",
      entry_code
    ),
    entry_date = if_else(
      is.na(entry_date) & entry_code == "N",
      birth_date,
      entry_date
    )
  )

animals_slaughter_farm1 <- animals_slaughter %>%
  filter(customer_id == farm_id) %>%
  distinct() %>%
  mutate(national_number = clean_ani(national_number),
         slaughter_date = as.Date(date)) %>%
  select(-c(created_at, customer_id, date))

animals_meta_farm1 <- animals_meta_farm1 %>%
  left_join(animals_slaughter_farm1,
            by = "national_number")


# clean dates
animals_meta_farm1 <- animals_meta_farm1 %>%
  mutate(date = as.Date(date),
         birth_date = as.Date(birth_date),
         entry_date = as.Date(entry_date),
         exit_date = as.Date(exit_date))

# Create variable for age_at_exit
animals_meta_farm1 <- animals_meta_farm1 %>%
  mutate(age_at_exit = if_else(
    !is.na(exit_date),
    interval(birth_date, exit_date) %/% months(1),
    NA_integer_
  ))

# Filter the metadata to dairy cows
dairy_meta_farm1 <- animals_meta_farm1 %>%
  mutate(category = str_trim(category)) %>%
  filter(category != "MA") %>%
  arrange(national_number, desc(date)) %>%  # most recent record first
  group_by(national_number) %>%
  slice(1) %>%  # keep only the most recent row per cow
  ungroup()

# Run any necessary manual correction from /lib

cache("dairy_meta_farm1")

# Check the quality of animal national numbers and metadata in Lely
HemAnimal <- HemAnimal %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         AniBirthday = as.Date(AniBirthday),
         AniMotherLifeNumber = clean_ani(AniMotherLifeNumber)) %>%
  distinct()

HemAnimal %>%
  count(AniLifeNumber) %>%
  filter(n > 1)

lely_supa_animal_diff <- dairy_meta_farm1 %>%
  anti_join(HemAnimal, by = c("national_number" = "AniLifeNumber"))

# ------------------------------------------------------------------------------
# Quick Checks
# ------------------------------------------------------------------------------

# Check distribution of entry_code
table(animals_meta_farm1$entry_code)

# Count missing values
cat("Missing entry_code: ", sum(is.na(animals_meta_farm1$entry_code)), "\n")
cat("Missing entry_date: ", sum(is.na(animals_meta_farm1$entry_date)), "\n")
cat("Missing exit_code: ", sum(is.na(animals_meta_farm1$exit_code)), "\n")
cat("Missing exit_date: ", sum(is.na(animals_meta_farm1$exit_date)), "\n")

# Check to ensure that all cows who have a slaughter date have an exit date
animals_meta_farm1 %>%
  filter(!is.na(slaughter_date) & is.na(exit_date)) %>%
  summarise(n_cows = n())

