# ------------------------------------------------------------------------------
# Script Name:    03_save_farm_tables.R
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

# Load farm config (define `farm_id`, `customer_id`, etc.)
source(here::here("config", "farm5_config.R"))

# Load functions
source(here::here("lib", "helpers.R"))

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

