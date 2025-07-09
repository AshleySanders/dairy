# Title: Save SQL Tables for Dairy Project
# Description: This script loads milk production data from a Lely database, along with animal metadata and lactation records.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily


# Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

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


# Load animal metadata
HemAnimal <- dbGetQuery(lely, "
  SELECT
    AniId,
    AniLifeNumber,
    AniActive,
    AniGenId,
    AniBirthday
  FROM HemAnimal
  ORDER BY AniId")

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

# Join milk data to cow identity info
milk_cows <- milk_all %>%
  left_join(
    HemAnimal %>% select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday),
    by = c("MdpAniId" = "AniId")
  )


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

animals <- dbGetQuery(prod, "
  SELECT
    birth_date,
    country_code,
    national_number,
    race
  FROM animals
  ORDER BY national_number")

animals_meta <- animals_history %>%
  left_join(animals, by = c("animal" = "national_number"))

# Save animals_slaughter table from Supabase for later use
animals_slaughter <- dbGetQuery(prod, "
  SELECT
    national_number,
    date,
    weight
  FROM animals_slaughter
  ORDER BY national_number, date")

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

head(gl_entries_farm1$label)
# Check the unique labels to understand what types of entries we have
unique(gl_entries_farm1$label)

# Exploring sales data for farm 1

farm1_gl_credits <- gl_entries_farm1 %>%
  filter(!is.na(credit), credit > 0)