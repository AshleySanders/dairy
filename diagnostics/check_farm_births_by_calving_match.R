# ------------------------------------------------------------------------------
# Script Name:    01_check_farm_births_by_calving_match.R
# Project:        Cockpit Agriculture – Herd Management Strategy
# Purpose:        Validate whether calf birthdates match mother calving dates
#
# Description:    Uses lactation_animal and animals_meta_farm1 to explore whether
#                 calves were born on the farm based on calving date proximity.
#                 NOT used for feature creation; retained for QA and analysis.
#
# Inputs:
#   - lactation_animal (from cache)
#   - animals_meta_farm1 (from cache)
#
# Outputs:
#   - Table of mother–calf pairs with calving–birth proximity
#
# Author:         Ashley Sanders
# Last updated:   2025-07-17
# ------------------------------------------------------------------------------


# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

# Clean columns of data
lactation_animal <- lactation_animal %>%
  mutate(
    AniLifeNumber = clean_ani(AniLifeNumber),
    AniMotherLifeNumber = clean_ani(AniMotherLifeNumber),
    AniBirthday = as.Date(AniBirthday),
    LacCalvingDate = as.Date(LacCalvingDate)
  )


# Create a look-up of mother calving dates
mother_calvings <- lactation_animal %>%
  select(AniLifeNumber, LacCalvingDate) %>%
  rename(mother_id = AniLifeNumber, mother_calving_date = LacCalvingDate)


# Identify calves with any matching birth
calf_birth_check <- animals_meta_farm1 %>%
  filter(!is.na(AniMotherLifeNumber)) %>%
  rename(
    calf_id = AniLifeNumber,
    calf_birthdate = AniBirthday,
    mother_id = AniMotherLifeNumber
  ) %>%
  left_join(mother_calvings, by = "mother_id", relationship = "many-to-many") %>%
  mutate(
    days_diff = as.numeric(difftime(calf_birthdate, mother_calving_date, units = "days")),
    born_on_farm = between(days_diff, 0, 2)  # Allow up to 2 days difference
  )

# Then summarize per calf
calves_born_on_farm <- calf_birth_check %>%
  filter(born_on_farm) %>%
  distinct(calf_id) %>%
  pull(calf_id)