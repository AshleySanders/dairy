# ------------------------------------------------------------------------------
# Script Name:    04_save_sql_tables.R
# Project:        Dairy Herd Management Strategy
# Purpose:        Load and cache raw data from Lely and Supabase databases
#
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
# Last updated:   2025-08-20
# ------------------------------------------------------------------------------

# Clean animal ID formatting
clean_ani <- function(x) str_replace_all(str_trim(as.character(x)), " ", "")

# Load farm config (define `farm_id`, `customer_id`, etc.)
source(here::here("config", "farm1_config.R"))

# Wrapper
assign_and_cache <- function(obj_name, value, prefix = farm_prefix) {
  full_name <- paste0(prefix, "_", obj_name)
  assign(full_name, value, envir = .GlobalEnv)
  cache(full_name)
}

# Load milk production data from Lely
assign_and_cache("milk_all", dbGetQuery(lely, "
SELECT MdpId, MdpAniId, MdpProductionDate, MdpDayProduction,
  MdpDayProductionMAvg, MdpMilkings, MdpFatPercentage, MdpProteinPercentage
FROM PrmMilkDayProduction
ORDER BY MdpAniId, MdpProductionDate"))


# Load animal metadata
assign_and_cache("HemAnimal", dbGetQuery(lely, "
SELECT AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday,
  AniMotherLifeNumber
FROM HemAnimal
ORDER BY AniId"))

# ! Run any necessary manual corrections from /lib

# Load lactation records
assign_and_cache("lactation", dbGetQuery(lely, "
SELECT LacId, LacAniId, LacNumber, LacDryOffDate, LacCalvingDate,
  LacColostrumDate, LacRemarks
FROM RemLactation"))

# Join milk data to cow identity info with the corrected HemAnimal data
assign_and_cache("milk_cows",
                 get(paste0(farm_prefix, "_milk_all")) %>%
                   left_join(
                     get(paste0(farm_prefix, "_HemAnimal")) %>%
                       select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday, AniMotherLifeNumber),
                     by = c("MdpAniId" = "AniId")
                   )
)

# Join lactation data from Lely with animal metadata
assign_and_cache("lactation_animal", dbGetQuery(lely, "
SELECT r.LacId, r.LacAniId, r.LacNumber, r.LacDryOffDate, r.LacCalvingDate,
  r.LacColostrumDate, r.LacRemarks,
  h.AniLifeNumber, h.AniBirthday, h.AniKeep, h.AniGenId, h.AniActive,
  h.AniMotherLifeNumber
FROM RemLactation r
INNER JOIN HemAnimal h ON h.AniId = r.LacAniId
ORDER BY h.AniId"))


# Get insemination data
assign_and_cache("insemination",
                 dbGetQuery(lely, "
SELECT i.InsId, i.InsSirId, i.InsNumber, i.InsRemarks, i.InsDate, i.InsLacId,
h.AniId, h.AniLifeNumber, h.AniBirthday, h.AniGenId, h.AniMotherLifeNumber
FROM RemInsemination i
INNER JOIN RemLactation r ON r.LacId = i.InsLacId
INNER JOIN HemAnimal h ON h.AniId = r.LacAniId
ORDER BY h.AniId") %>%
                   mutate(
                     AniLifeNumber = clean_ani(AniLifeNumber),
                     AniBirthday = as.Date(AniBirthday),
                     InsDate = as.Date(InsDate)
                   )
)

# Get pregnancy data
assign_and_cache("pregnancy", dbGetQuery(lely, "SELECT PreLacId, PreDate, PreInsId, PreRemark FROM RemPregnancy"))
cache("pregnancy")

# Health Data from Lely
assign_and_cache("animal_health", dbGetQuery(lely, "
SELECT
  d.DisId, d.DisDcaId, d.DisName, d.DisCurePeriod, d.DisDescription,
  diag.DiaAniId, diag.DiaDate, diag.DiaRemarks, diag.DiaWithMilk,
  diag.DiaWithMeat,
  h.AniId, h.AniLifeNumber
FROM LimDisease d
LEFT JOIN HemDiagnoses diag ON d.DisId = diag.DiaDisId
LEFT JOIN HemAnimal h ON diag.DiaAniId = h.AniId"))

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


# Save animals_slaughter table from Supabase for later use
animals_slaughter <- dbGetQuery(prod, "
  SELECT
    national_number,
    date,
    weight,
    created_at,
    customer_id,
    selling_price,
    classification,
    category
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

assign_and_cache("gl_entries", dbGetQuery(prod, paste0(
  "SELECT * FROM gl_entries WHERE customer_id = '", customer_id, "' ORDER BY date DESC")))

# Exploring sales data for farm 1

assign_and_cache("gl_credits",
                 get(paste0(farm_prefix, "_gl_entries")) %>%
                   filter(!is.na(credit), credit > 0)
)
