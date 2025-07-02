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