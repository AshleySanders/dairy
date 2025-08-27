# ------------------------------------------------------------------------------
# Script Name:    02_reproduction.R
# Project:        Cockpit Agriculture – Herd Management Strategy
#
# Purpose:        Construct a clean, cow-level dataset of reproductive events,
#                 combining lactation, insemination, and pregnancy records.
#
# Description:    This script performs the following:
#                 - Cleans and standardizes animal identifiers
#                 - Attaches each insemination to a lactation cycle
#                 - Joins pregnancy confirmation data to insemination records
#                 - Deduplicates pregnancies (latest confirmation per InsId)
#                 - Flags successful inseminations (pregnancy confirmed)
#                 - Flags successful pregnancies (confirmed + led to calving)
#                 - Adds birth_date from validated Supabase metadata
#
# Inputs:
#   - Cache: insemination, pregnancy
#   - CSV:   data/lactation_summary_all.csv (from SQL script)
#   - Table: dairy_meta_farm1 (de-duplicated Supabase metadata)
#
# Outputs:
#   - data/insem_lac_preg.rds: Cleaned and flagged dataset (RDS)
#   - data/insem_lac_preg.csv: Exported copy (CSV)
#
# Author:         Ashley Sanders
# Created:        2025-06-19
# Last Updated:   2025-08-21
# ------------------------------------------------------------------------------


# Load config and helpers
source(here::here("config", "farm5_config.R"))
source(here::here("lib", "helpers.R"))

# Load lactation metrics
assign(paste0(farm_prefix, "_lactation_summary_all"), read.csv(here("data", "fm5_lactation_cycles_milk_metrics.csv")))


# Clean and prepare lactation summary
assign(paste0(farm_prefix, "_lactation_summary"),
       get(paste0(farm_prefix, "_lactation_summary_all")) %>%
         mutate(AniLifeNumber = clean_ani(AniLifeNumber),
                AniId = clean_ani(AniId),
                LacCalvingDate = as.Date(LacCalvingDate),
                AniId = as.character(AniId)) %>%
         left_join(
           get(paste0(farm_prefix, "_dairy_meta")) %>% select(-c(AniId, AniActive)),
           by = c("AniLifeNumber" = "national_number"), relationship = "many-to-many"
         ) %>%
         mutate(
           LacColostrumDate = as.Date(LacColostrumDate),
           milk_production_start_date = as.Date(milk_production_start_date),
           milk_production_end_date = as.Date(milk_production_end_date),
           avg_daily_yield = as.numeric(avg_daily_yield),
           mean_fat_percent = as.numeric(mean_fat_percent),
           mean_protein_percent = as.numeric(mean_protein_percent),
           dry_off_date = as.Date(dry_off_date),
           dry_off_interval = as.integer(dry_off_interval)
         ) %>%
         select(-c(category, customer_id, date, month, year))
)


# Ensure correct data types
glimpse(get(paste0(farm_prefix, "_lactation_summary")))


# Checks
dim(get(paste0(farm_prefix, "_lactation_summary_all")))
dim(get(paste0(farm_prefix, "_lactation_summary")))
colnames(get(paste0(farm_prefix, "_lactation_summary")))
sum(is.na(get(paste0(farm_prefix, "_lactation_summary"))$LacId))

# Deduplicate on LacId to avoid inflation
assign(paste0(farm_prefix, "_lactation_summary"),
       get(paste0(farm_prefix, "_lactation_summary")) %>%
         arrange(desc(!is.na(AniMotherLifeNumber))) %>%
         group_by(LacId) %>%
         slice(1) %>%
         ungroup()
)

cache(paste0(farm_prefix, "_lactation_summary"))

# verify that the number of rows matches the original lactation_summary_all table
nrow(get(paste0(farm_prefix, "_lactation_summary_all"))) == nrow(get(paste0(farm_prefix, "_lactation_summary")))

# Join lactation and insemination data to have one row per insemination attempt for each lactation cycle for each cow.

# Prepare insemination and pregnancy tables
assign(paste0(farm_prefix, "_pregnancy"),
       get(paste0(farm_prefix, "_pregnancy")) %>% mutate(PreDate = as.Date(PreDate))
)


assign(paste0(farm_prefix, "_insemination"), get(paste0(farm_prefix, "_insemination")))


# Join insemination with lactation
assign(paste0(farm_prefix, "_insem_lactation"),
       get(paste0(farm_prefix, "_lactation_summary")) %>%
         left_join(
           get(paste0(farm_prefix, "_insemination")) %>%
             select(-c(AniBirthday, AniGenId, AniMotherLifeNumber, AniId)),
           by = c("AniLifeNumber", "LacId" = "InsLacId")
         )
)

# Add insemination data that produced the first lactation cycles in lactation_summary even though all the milking records will be null for these rows.

first_calvings <- fm5_lactation_summary %>%
  filter(!is.na(LacCalvingDate)) %>%
  group_by(AniLifeNumber) %>%
  slice_min(order_by = LacCalvingDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(AniLifeNumber, first_calving = as.Date(LacCalvingDate))

pre_first_cycle_insem <- fm5_insemination %>%
  mutate(InsDate = as.Date(InsDate)) %>%
  inner_join(first_calvings, by = "AniLifeNumber") %>%
  mutate(first_calving = as.Date(first_calving)) %>%
  filter(
    InsDate < as.Date("2013-03-01")) %>%
  group_by(AniLifeNumber) %>%
  mutate(max_pre_calving_lacid = if (all(is.na(InsLacId))) NA_integer_ else max(InsLacId, na.rm = TRUE)) %>%
  filter(!is.na(max_pre_calving_lacid), InsLacId == max_pre_calving_lacid) %>%
  ungroup() %>%
  select(-max_pre_calving_lacid, -first_calving)

# Checks
names(get(paste0(farm_prefix, "_insem_lactation")))
names(pre_first_cycle_insem)

# Handle mismatch between column names of the table with the new rows to add and the original table

# target object name
target_name <- paste0(farm_prefix, "_insem_lactation")

# get the current table
insem_lac <- get(target_name)

# harmonize the pre-first-cycle rows to the target schema
pre_harmonized <- pre_first_cycle_insem %>%
  dplyr::rename(LacId = InsLacId) %>%          # align key name
  dplyr::mutate(source = "pre_first_cycle") %>%# optional provenance
  # keep only columns that exist in the target; missing ones will be NA on bind
  dplyr::select(dplyr::any_of(names(insem_lac))) %>%
  mutate(AniId = as.character(AniId))

# bind, preserving target's column order
combined <- dplyr::bind_rows(insem_lac, pre_harmonized) %>%
  dplyr::select(dplyr::all_of(names(insem_lac)))

assign(paste0(farm_prefix, "_insem_lactation_full"), combined)


# Checks
dim(get(paste0(farm_prefix, "_insem_lactation_full")))
colnames(get(paste0(farm_prefix, "_insem_lactation_full")))
glimpse(get(paste0(farm_prefix, "_insem_lactation_full")))

# Join pregnancy to insemination+lactation
assign(paste0(farm_prefix, "_insem_lac_preg"),
       get(paste0(farm_prefix, "_insem_lactation_full")) %>%
         left_join(get(paste0(farm_prefix, "_pregnancy")), by = c("InsId" = "PreInsId"), relationship = "many-to-many")
)

# Checks
dim(get(paste0(farm_prefix, "_insem_lac_preg")))
colnames(get(paste0(farm_prefix, "_insem_lac_preg")))

# View(fm1_insem_lac_preg %>% filter(AniLifeNumber == "FR4404288134")) # visual exam

# De-duplicate when there are multiple pregnancy confirmation dates  on InsId (keep latest confirmation)
assign(paste0(farm_prefix, "_insem_lac_preg_dedup"),
       get(paste0(farm_prefix, "_insem_lac_preg")) %>%
         group_by(InsId) %>%
         arrange(desc(PreDate)) %>%
         slice_head(n = 1) %>%
         ungroup()
)

# Flag successful inseminations and pregnancies
assign(paste0(farm_prefix, "_insem_lac_preg"),
       get(paste0(farm_prefix, "_insem_lac_preg")) %>%
         mutate(
           successful_insem = !is.na(PreDate),
           successful_pregnancy = !is.na(PreDate) & !is.na(LacCalvingDate)
         )
)


# Save to disk and cache
write.csv(get(paste0(farm_prefix, "_insem_lac_preg")), here("data", paste0(farm_prefix, "_insem_lac_preg.csv")))
saveRDS(get(paste0(farm_prefix, "_insem_lac_preg")), here("data", paste0(farm_prefix, "_insem_lac_preg.rds")))
cache(paste0(farm_prefix, "_insem_lac_preg"))