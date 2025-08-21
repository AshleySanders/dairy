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
source(here::here("config", "farm1_config.R"))
source(here::here("lib", "helpers.R"))

# Load lactation metrics
assign(paste0(farm_prefix, "_lactation_summary_all"), read.csv(here("data", "fm1_lactation_cycles_milk_metrics.csv")))


# Clean and prepare lactation summary
assign(paste0(farm_prefix, "_lactation_summary"),
       get(paste0(farm_prefix, "_lactation_summary_all")) %>%
         mutate(AniLifeNumber = clean_ani(AniLifeNumber),
                AniId = clean_ani(AniId),
                LacCalvingDate = as.Date(LacCalvingDate),
                AniId = as.character(AniId)) %>%
         mutate(
           AniLifeNumber = case_when(
             AniId == "253" ~ "FR4404288298",
             AniId == "257" ~ "FR4404288299",
             AniId == "267" ~ "FR4404288307",
             AniId == "277" ~ "FR4404288308",
             AniId == "282" ~ "FR4404288319",
             AniId == "289" ~ "FR4404288318",
             AniId == "286" ~ "FR4404288320",
             AniId == "987" ~ "FR4404288598",
             AniId == "1040" ~ "FR4404288645",
             AniId == "1064" ~ "FR4404288667",
             AniId == "1071" ~ "FR4404288645",
             TRUE ~ AniLifeNumber
           ),
           AniId = as.integer(AniId)
         ) %>%
         left_join(
           get(paste0(farm_prefix, "_dairy_meta")) %>% select(-AniId, AniActive),
           by = "AniLifeNumber"
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
         )
)


# Ensure correct data types
glimpse(lactation_summary)


# Checks
dim(lactation_summary_all)
dim(lactation_summary)
colnames(lactation_summary)
sum(is.na(lactation_summary$LacId))

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
# nrow(lactation_summary_all) == nrow(lactation_summary_dedup)

# After verification save the de-duped table as the original table name
# lactation_summary <- lactation_summary_dedup

# Join lactation and insemination data to have one row per insemination attempt for each lactation cycle for each cow.

# Prepare insemination and pregnancy tables
assign(paste0(farm_prefix, "_pregnancy"),
       get(paste0(farm_prefix, "_pregnancy")) %>% mutate(PreDate = as.Date(PreDate))
)


assign(paste0(farm_prefix, "_insemination"), get(paste0(farm_prefix, "_insemination")))


# Join insemination with lactation
assign(paste0(farm_prefix, "_insem_lactation"),
       get(paste0(farm_prefix, "_insemination")) %>%
         left_join(
           get(paste0(farm_prefix, "_lactation_summary")) %>%
             select(-c(AniBirthday, AniGenId, AniMotherLifeNumber, AniId,
                       milk_production_start_date,
                       milk_production_end_date, lactation_duration,
                       total_milk_production,
                       avg_daily_yield, early_lactation_yield,
                       mid_lactation_yield,
                       delta_early_mid_yield, mean_fat_percent,
                       mean_protein_percent)),
           by = c("AniLifeNumber", "InsLacId" = "LacId")
         )
)

# Checks
# dim(insem_lactation)
# colnames(insem_lactation)
# Join pregnancy to insemination+lactation
assign(paste0(farm_prefix, "_insem_lac_preg"),
       get(paste0(farm_prefix, "_insem_lactation")) %>%
         left_join(get(paste0(farm_prefix, "_pregnancy")), by = c("InsId" = "PreInsId"), relationship = "many-to-many")
)

# Checks
# dim(insem_lac_preg)
# colnames(insem_lac_preg)
# View(insem_lac_preg %>% filter(AniLifeNumber == "FR4404288134")) # visual exam

# De-duplicate when there are multiple pregnancy confirmation dates

# Deduplicate on InsId (keep latest confirmation)
assign(paste0(farm_prefix, "_insem_lac_preg"),
       get(paste0(farm_prefix, "_insem_lac_preg")) %>%
         group_by(InsId) %>%
         arrange(desc(PreDate)) %>%
         slice_head(n = 1) %>%
         ungroup()
)

# Check
# nrow(insem_lactation) == nrow(insem_lac_preg_dedup)


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