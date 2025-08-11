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

animals_history_farm1 <- animals_history_farm1 %>%
  filter(date < as.Date("2024-09-19")) # limit this dataframe to match the data coming from the last Lely back-up

animals_history_farm1 <- animals_history_farm1 %>%
  distinct() %>%
  mutate(national_number = clean_ani(animal),
         date = as.Date(date),
         entry_date = as.Date(entry_date),
         exit_date = as.Date(exit_date)) %>%
  select(-animal)


# Add slaughter information
animals_slaughter_farm1 <- animals_slaughter %>%
  filter(customer_id == farm_id) %>%
  distinct() %>%
  mutate(AniLifeNumber = clean_ani(national_number),
         slaughter_date = as.Date(date),
         classification = case_when(
           classification == "O=3" ~ "O-3",
           classification == "P+2" ~ "P-2",
           classification == "P+3" ~ "P-3",
           classification == "P+4" ~ "P-4",
           classification == "P=2" ~ "P-2",
           classification == "P=3" ~ "P-3"
           )
         ) %>%
  select(-c(created_at, customer_id, date))



# Filter the metadata to dairy cows
dairy_history_farm1 <- animals_history_farm1 %>%
  mutate(category = str_trim(category)) %>%
  filter(category != "MA") %>%
  arrange(national_number, desc(date)) %>%  # most recent record first
  group_by(national_number) %>%
  slice(1) %>%  # keep only the most recent row per cow
  ungroup()

# Run any necessary manual correction from /lib

cache("dairy_history_farm1")

# Check the quality of animal national numbers and metadata in Lely
HemAnimal <- HemAnimal %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         AniBirthday = as.Date(AniBirthday),
         AniMotherLifeNumber = clean_ani(AniMotherLifeNumber)) %>%
  distinct()

dairy_meta_farm1 %>%
  count(AniLifeNumber) %>%
  filter(n > 1)

# Capture all national numbers of cows in the herd
herd_ids <- unique(HemAnimal$AniLifeNumber)

dairy_history_farm1 <- dairy_history_farm1 %>%
  filter(national_number %in% herd_ids)

dairy_meta_farm1 <- HemAnimal %>%
  left_join(dairy_history_farm1, by = c("AniLifeNumber" = "national_number"))

# Add an "N" entry_code for cows whose mothers were also in the herd
dairy_meta_farm1 <- dairy_meta_farm1 %>%
  mutate(
    entry_code = if_else(
      is.na(entry_code) & AniMotherLifeNumber %in% herd_ids,
      "N",
      entry_code
    ),
    entry_date = if_else(
      is.na(entry_date) & entry_code == "N",
      AniBirthday,
      entry_date
    )
  )

dairy_meta_farm1 <- dairy_meta_farm1 %>%
 # Compute a completeness score: count of non-missing important columns
  rowwise() %>%
  mutate(
    completeness = sum(
      !is.na(exit_date),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%

  # 3. For each cow, keep the row with the highest completeness
  group_by(AniLifeNumber) %>%
  slice_max(order_by = completeness, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-completeness)

# Limit the metadata table to cows with milking data within the date range of interest.
valid_cow_ids <- unique(lactation_metrics$AniLifeNumber)

dairy_meta_farm1 <- dairy_meta_farm1 %>%
  filter(AniLifeNumber %in% valid_cow_ids)

# ------------------------------------------------------------------------------
# Quick Checks
# ------------------------------------------------------------------------------

# Check distribution of entry_code
table(dairy_meta_farm1$entry_code)

# Count missing values
cat("Missing entry_code: ", sum(is.na(dairy_meta_farm1$entry_code)), "\n")
cat("Missing entry_date: ", sum(is.na(dairy_meta_farm1$entry_date)), "\n")
cat("Missing exit_code: ", sum(is.na(dairy_meta_farm1$exit_code)), "\n")
cat("Missing exit_date: ", sum(is.na(dairy_meta_farm1$exit_date)), "\n")

cache("dairy_meta_farm1")

# Create a subset of animal_health data for dairy cows
animal_health <- animal_health %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber))

dairy_health <- animal_health %>%
  filter(AniLifeNumber %in% herd_ids)

cache("dairy_health")
