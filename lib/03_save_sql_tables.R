# ------------------------------------------------------------------------------
# Script Name:    load_and_cache_sql_data.R
# Project:        Cockpit Agriculture – Herd Management Strategy
# Purpose:        Load and cache raw data from Lely and Supabase databases to
#                 avoid redundant SQL queries and support faster, reproducible
#                 development workflows within ProjectTemplate.
#
# Description:    This script connects to the Lely milking system and Supabase
#                 databases to retrieve core tables related to:
#                   - Milk production (daily and monthly)
#                   - Animal metadata
#                   - Lactation, insemination, and pregnancy records
#                   - Farm accounting (general ledger)
#                   - Mil’Klic summaries (if available)
#                 Retrieved data is immediately saved to `cache/` using the
#                 `cache()` function. These cached datasets will be loaded
#                 automatically by ProjectTemplate if not re-generated.
#
# Usage Notes:
#   - This script is designed to be run manually when updates to raw SQL data
#     are needed (e.g., new records in Lely or Supabase).
#   - Do NOT place this script in `munge/`, as it should not re-run at each
#     call to `load.project()`.
#
# Inputs:
#   - Live SQL connections to `lely` (milking system) and `prod` (Supabase)
#
# Outputs (cached R objects):
#   - milk_all, HemAnimal, lactation, lactation_animal, insemination, pregnancy
#   - animals_history, animals, animals_meta, animals_slaughter
#   - mk_animals_lactations, mk_animals_reproductions, mk_animals_reproductions_calves
#   - mk_animals_reproductions_events, gl_entries, gl_entries_farm1, farm1_gl_credits
#
# Author:         Ashley Sanders
# Created:        2025-06-25
# Last updated:   2025-07-16
# ------------------------------------------------------------------------------

# Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

# Function to clean AniLifeNumber so that it matches the format of national_number/animal from Supabase tables
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")


# Load milk production data from Lely
milk_all <- dbGetQuery(lely, "
  SELECT
    MdpId,
    MdpAniId,
    MdpProductionDate,
    MdpDayProduction,
    MdpDayProductionMAvg,
    MdpMilkings,
    MdpFatPercentage,
    MdpProteinPercentage
  FROM PrmMilkDayProduction
  ORDER BY MdpAniId, MdpProductionDate
")

cache("milk_all")


# Load animal metadata
HemAnimal <- dbGetQuery(lely, "
  SELECT
    AniId,
    AniLifeNumber,
    AniActive,
    AniGenId,
    AniBirthday,
    AniMotherLifeNumber
  FROM HemAnimal
  ORDER BY AniId")

cache("HemAnimal")

# Load lactation records
lactation <- dbGetQuery(lely, "
  SELECT
    LacId,
    LacAniId,
    LacNumber,
    LacDryOffDate,
    LacCalvingDate,
    LacColostrumDate,
    LacRemarks
  FROM RemLactation
")

cache("lactation")

# Join milk data to cow identity info
milk_cows <- milk_all %>%
  left_join(
    HemAnimal %>% select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday, AniMotherLifeNumber),
    by = c("MdpAniId" = "AniId")
  )

cache("milk_cows")


# Join lactation data from Lely with animal metadata
lactation_animal <- dbGetQuery(lely, "
  SELECT
    RemLactation.LacId,
    RemLactation.LacAniId,
    RemLactation.LacNumber,
    RemLactation.LacDryOffDate,
    RemLactation.LacCalvingDate,
    RemLactation.LacColostrumDate,
    RemLactation.LacRemarks,
    HemAnimal.AniLifeNumber,
    HemAnimal.AniBirthday,
    HemAnimal.AniKeep,
    HemAnimal.AniGenId,
    HemAnimal.AniActive,
    HemAnimal.AniMotherLifeNumber
  FROM RemLactation
  INNER JOIN HemAnimal
    ON HemAnimal.AniId = RemLactation.LacAniId
  ORDER BY HemAnimal.AniId
")

cache("lactation_animal")

# Get insemination data
insemination <- dbGetQuery(lely, "
  SELECT
    RemInsemination.*,
    HemAnimal.AniLifeNumber,
    HemAnimal.AniBirthday,
    HemAnimal.AniKeep,
    HemAnimal.AniGenId,
    HemAnimal.AniActive,
    HemAnimal.AniMotherLifeNumber
  FROM RemInsemination
  INNER JOIN RemLactation
    ON RemLactation.LacId = RemInsemination.InsLacId
  INNER JOIN HemAnimal
    ON HemAnimal.AniId = RemLactation.LacAniId
  ORDER BY HemAnimal.AniId
")

# Convert date columns to Date type
insemination <- insemination %>%
  mutate(
    AniBirthday = as.Date(AniBirthday, format = "%Y-%m-%d"),
    InsDate = as.Date(InsDate, format = "%Y-%m-%d")
  )

cache("insemination")

# Get pregnancy data
pregnancy <- dbGetQuery(lely, "
    SELECT PreLacId, PreDate, PreInsId, PreRemark
    FROM RemPregnancy
")

cache("pregnancy")

# Save animals_history table from Supabase for later use

animals_history <- dbGetQuery(prod, "
  SELECT
  	animal,
		category,
		customer_id,
		date,
		entry_code,
		entry_date,
		exit_code,
		exit_date,
		month,
		year
	FROM animals_history
	ORDER BY animal")

cache("animals_history")

animals <- dbGetQuery(prod, "
  SELECT
    created_at,
    customer_id,
    birth_date,
    country_code,
    national_number,
    race,
    mother
  FROM animals
  ORDER BY national_number")

cache("animals")

animals_meta <- animals_history %>%
  left_join(animals, by = c("animal" = "national_number"))

animals_meta <- animals_meta %>%
  mutate(animal = clean_ani(animal))

cache("animals_meta")


# Save animals_slaughter table from Supabase for later use
animals_slaughter <- dbGetQuery(prod, "
  SELECT
    national_number,
    date,
    weight,
    created_at,
    customer_id
  FROM animals_slaughter
  ORDER BY national_number, date")

cache("animals_slaughter")

milk_invoice <- dbGetQuery(prod, "
  SELECT *
  FROM milk_invoice")

# --- Save Mil'Klic tables from Supabase, denoted by "mk_" prefix to table names here ---

mk_animals_lactations <- dbGetQuery(prod, "
  SELECT
    national_number,
    customer_id,
    lactation_number,
    lactation_start_date,
    lactation_cause,
    lactation_classification,
    lactation_status,
    lactation_total_milk_production_kg,
    lactation_duration,
    lactation_milk_fat_ratio,
    lactation_milk_protein_ratio,
    lactation_milk_fat_kg,
    lactation_milk_protein_kg,
    lactation_dry_off_date,
    lactation_qualification,
    lactation_305_days_milk_kg,
    lactation_305_days_fat_ratio,
    lactation_305_days_protein_ratio,
    lactation_305_days_fat_kg,
    lactation_305_days_protein_kg,
    lactation_305_days_type_taris,
    lactation_305_days_date_taris,
    lactation_msu
  FROM animals_lactations
")

cache("mk_animals_lactations")

mk_animals_reproductions <- dbGetQuery(prod, "
  SELECT
    customer_id,
    national_number,
    lactation_number,
    calving_date,
    event,
    insem_attempts,
    calving_to_insem_days,
    calving_to_pregnancy_days
  FROM animals_reproductions
")

cache("mk_animals_reproductions")

mk_animals_reproductions_calves <- dbGetQuery(prod, "
  SELECT
    reproduction_id,
    customer_id,
    calf_name,
    calf_number,
    calf_birthdate,
    calf_sex,
    calf_race,
    birth_condition
  FROM animals_reproductions_calves
")

cache("mk_animals_reproductions_calves")

mk_animals_reproductions_events <- dbGetQuery(prod, "
  SELECT
    customer_id,
	  reproduction_id,
	  insem_event_code,
	  insem_event_date,
	  insem_preg_diagnosis,
	  insem_preg_diagnosis_type,
	  insem_diagnosis_detail,
	  insem_attempt_number,
	  insem_end,
	  insem_sire_name,
	  insem_sire_race,
	  insem_sire_detail,
	  days_since_last_insem
  FROM animals_reproductions_events
")

cache("mk_animals_reproductions_events")

# --- Supabase Tables ---
gl_entries <- dbGetQuery(prod, "
  SELECT
    journal,
		identifier,
		date,
		counterpart,
		label,
		number,
		quantity,
		unit_price,
		debit,
		credit,
		balance,
		gl_account_number,
		customer_id,
		balance_num,
		start_fiscal_year,
		end_fiscal_year,
		document_id,
		invoice_id
	FROM gl_entries")

cache("gl_entries")

# Grand Livre entries for the first farm in the analysis

gl_entries_farm1 <- dbGetQuery(prod, "
  SELECT
    *
  FROM
    gl_entries
  WHERE
    customer_id = '16450bc2-f930-4052-a3f7-a602646e64cc'
  ORDER BY
    date DESC")

cache("gl_entries_farm1")

head(gl_entries_farm1$label)
# Check the unique labels to understand what types of entries we have
unique(gl_entries_farm1$label)

# Exploring sales data for farm 1

farm1_gl_credits <- gl_entries_farm1 %>%
  filter(!is.na(credit), credit > 0)

cache("farm1_gl_credits")

# Prepare a dataframe with each cow's history and mother's national number (AniLifeNumber)

animals_meta_farm1 <- animals_meta %>%
  filter(customer_id == "16450bc2-f930-4052-a3f7-a602646e64cc",
         race == "66") %>%
  mutate(animal = clean_ani(animal),
         entry_date = as.Date(entry_date),
         exit_date = as.Date(exit_date)) %>%
  select(animal, entry_code, entry_date, exit_code, exit_date) %>%
  distinct(animal, .keep_all = TRUE)

lely_animal <- HemAnimal %>%
  mutate(AniLifeNumber = clean_ani(AniLifeNumber),
         AniMotherLifeNumber = clean_ani(AniMotherLifeNumber))

animals_meta_farm1 <- lely_animal %>%
  left_join(animals_meta_farm1, by = c("AniLifeNumber" = "animal"))

cache("animals_meta_farm1")
