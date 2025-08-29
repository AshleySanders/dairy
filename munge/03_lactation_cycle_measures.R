# ─────────────────────────────────────────────────────────────────────────────
# Script Name: 03-lactation_metrics.R
# Author: Ashley Sanders
# Date Created: 2025-07-10
# Last Updated: 2025-08-28
# Project: Herd Management Strategy Analysis – Dairy Cow Lactation Cycle Metrics
#
# Description:
# This script generates per-lactation-cycle metrics for dairy cows using joined
# lactation, insemination, and reproductive outcome data. It computes performance
# and decision-related indicators used in profitability modeling and cluster analysis.
#
# Key Metrics Calculated (by lactation cycle):
# - age_at_calving (months)
# - n_insem: number of artificial insemination attempts
# - n_failed_insem: number of failed insemination attempts
# - n_failed_pregnancies: number of pregnancies that failed
# - successful/total insemination ratio (implied via n_insem, n_failed_insem)
# - calving_to_insem_days: days from calving to first insemination
# - calving_interval_days: time between calvings
# - dry_off_interval: time between milk end and next cycle
# - last_lactation: flag for last lactation cycle (based on exit data)
#
# Milk Production Metrics (imported from SQL & estimated delivery):
# - total_milk_production, avg_daily_yield
# - early_lactation_yield, mid_lactation_yield
# - delta_early_mid_yield
# - mean_fat_percent, mean_protein_percent
# - Estimated delivered milk using 0.959 correction factor for retained milk
#
# Identifiers Retained:
# - AniId, AniLifeNumber, LacId, InsId, CalculatedLactationCycle
#
# Inputs:
# - lactation_summary (from SQL output; contains milk metrics and dates)
# - insem_lac_preg.rds (insemination and calving data, per cycle)
# - HemAnimal (AniBirthday used for age calculations)
#
# Outputs:
# - lactation_metrics: enhanced lactation-cycle level dataset cached to disk
#
# Dependencies:
# - dplyr, lubridate, tidyr, stringr
#
# Notes:
# - still_milking flag recalculated using exit_date and last milk_production_end_date
# - Some failed inseminations not shown in insem_lac_preg; deeper reproductive record inspection needed
# - Potential future enhancements: days_to_peak_yield
# ─────────────────────────────────────────────────────────────────────────────

# Load config and helpers
source(here::here("config", "farm5_config.R"))
source(here::here("lib", "helpers.R"))

# Determine if the cow is likely still milking, clean id numbers, and calculate estimated conserved and delivered milk quantities
# Prep input tables
assign(paste0(farm_prefix, "_lactation_summary"),
       get(paste0(farm_prefix, "_lactation_summary")) %>%
         mutate(
           AniLifeNumber = clean_ani(AniLifeNumber),
           AniMotherLifeNumber = clean_ani(AniMotherLifeNumber),
           still_milking = if_else(
             is.na(exit_date) & milk_production_end_date %in% as.Date(c("2024-09-18", "2024-09-19")),
             TRUE, FALSE
           ),
           est_deliver_total_milk_L = total_milk_production * 0.959,
           est_deliver_avg_daily_yield = avg_daily_yield * 0.959,
           est_deliver_early_lactation_yield = early_lactation_yield * 0.959,
           est_deliver_mid_lactation_yield = mid_lactation_yield * 0.959,
           est_deliver_delta_lactation_yield = delta_early_mid_yield * 0.959
         )
)


#--- Fill in AniBirthday where missing -----------------------------------------
assign(
  paste0(farm_prefix, "_lactation_summary"),
  get(paste0(farm_prefix, "_lactation_summary")) %>%
    group_by(AniLifeNumber) %>%
    mutate(AniBirthday = first(na.omit(AniBirthday))) %>%
    ungroup()
)

# --- Look up & replace missing birth dates ------------------------------------

animals_lookup <- get(paste0(farm_prefix, "_HemAnimal")) %>%
  mutate(
    AniLifeNumber = clean_ani(AniLifeNumber),
    AniBirthday   = as.Date(AniBirthday)
  ) %>%
  filter(!is.na(AniLifeNumber), AniLifeNumber != "") %>%
  group_by(AniLifeNumber) %>%
  summarise(
    # choose a single birthday per cow; earliest non-missing is a safe default
    AniBirthday_lookup = suppressWarnings(min(AniBirthday, na.rm = TRUE)),
    .groups = "drop"
  )


fill_birthdays_from_HemAnimal <- function(df, id_col = "AniLifeNumber", bday_col = "AniBirthday") {
  id_sym   <- rlang::sym(id_col)
  bday_sym <- rlang::sym(bday_col)

  df %>%
    mutate(
      !!id_sym   := clean_ani(!!id_sym),
      !!bday_sym := as.Date(!!bday_sym)
    ) %>%
    left_join(
      animals_lookup,
      by = setNames("AniLifeNumber", id_col)  # join: df[[id_col]] == animals_lookup$AniLifeNumber
    ) %>%
    mutate(
      !!bday_sym := dplyr::coalesce(!!bday_sym, .data$AniBirthday_lookup)
    ) %>%
    select(-AniBirthday_lookup)
}

target_name <- paste0(farm_prefix, "_lactation_summary")
df          <- get(target_name)

before_missing <- sum(is.na(df$AniBirthday))
message("Missing AniBirthday BEFORE: ", before_missing)

df_filled <- fill_birthdays_from_HemAnimal(df, id_col = "AniLifeNumber", bday_col = "AniBirthday")

after_missing <- sum(is.na(df_filled$AniBirthday))
message("Missing AniBirthday AFTER:  ", after_missing,
        " (filled: ", before_missing - after_missing, ")")

# Run lib/05_farm5_fix_missing_birthdays_lac_summary.R to identify and replace the final missing birth dates


# --- Use df_filled to update farmX_lactation_summary table---------------------

# Be sure to change the farm number if needed for other farms
target_name <- paste0(farm_prefix, "_lactation_summary")

assign(target_name, df_filled, envir = .GlobalEnv)

# Checks
sum(is.na(fm5_lactation_summary$AniLifeNumber))
sum(is.na(fm5_lactation_summary$AniBirthday))

# --- Calculate age at calving for each lactation cycle ------------------------
assign(paste0(farm_prefix, "_age_at_calving"),
       get(paste0(farm_prefix, "_lactation_summary")) %>%
         filter(RemLactation_LacNumber > 0, !is.na(AniBirthday), !is.na(LacCalvingDate), !is.na(LacId)) %>%
         mutate(age_at_calving = interval(AniBirthday, LacCalvingDate) %/% months(1)) %>%
         select(AniLifeNumber, LacId, age_at_calving) %>%
         group_by(AniLifeNumber, LacId) %>%
         summarise(age_at_calving = first(age_at_calving), .groups = "drop")
)

assign(paste0(farm_prefix, "_lactation_metrics"),
       get(paste0(farm_prefix, "_lactation_summary")) %>%
         left_join(get(paste0(farm_prefix, "_age_at_calving")), by = c("AniLifeNumber", "LacId"))
)

# Insemination counts
assign(paste0(farm_prefix, "_insem_per_lac"),
       get(paste0(farm_prefix, "_insemination")) %>%
         group_by(AniLifeNumber, InsLacId) %>%
         summarise(n_insem = max(InsNumber), .groups = "drop")
)


assign(paste0(farm_prefix, "_lactation_metrics"),
       get(paste0(farm_prefix, "_lactation_metrics")) %>%
         left_join(get(paste0(farm_prefix, "_insem_per_lac")), by = c("AniLifeNumber", "LacId" = "InsLacId"))
)

# First preg flag, # of failed inseminations, NA checks and flag reasons
assign(paste0(farm_prefix, "_lactation_metrics"),
       get(paste0(farm_prefix, "_lactation_metrics")) %>%
         mutate(
           first_pregnancy = RemLactation_LacNumber == 1,
           n_failed_insem = as.numeric(n_insem - 1),
           n_insem_flag = case_when(
             !is.na(n_insem) ~ "recorded",
             first_pregnancy ~ "first pregnancy",
             still_milking == 1 ~ "still milking",
             TRUE ~ "unknown"
           ),
           n_failed_insem = as.numeric(n_insem - 1)
         )
)

# Last lactation flag
assign(paste0(farm_prefix, "_lactation_metrics"),
       get(paste0(farm_prefix, "_lactation_metrics")) %>%
         group_by(AniLifeNumber) %>%
         mutate(
           max_lac_if_exited = if(any(!is.na(exit_date))) {
             max(RemLactation_LacNumber[!is.na(exit_date)], na.rm = TRUE)
           } else { NA_integer_ },
           last_lactation = if_else(
             !is.na(max_lac_if_exited) & RemLactation_LacNumber == max_lac_if_exited,
             TRUE, FALSE
           )
         ) %>%
         ungroup() %>%
         select(-max_lac_if_exited)
)


# Calving to insemination interval
assign(
  paste0(farm_prefix, "_calving_to_insem"),
  get(paste0(farm_prefix, "_insem_lac_preg")) %>%
    dplyr::mutate(
      InsDate = as.Date(InsDate),
      LacCalvingDate = as.Date(LacCalvingDate)
    ) %>%
    dplyr::group_by(AniLifeNumber, LacId) %>%
    dplyr::summarise(
      # Did we observe an InsNumber == 1 in this (cow, lactation)?
      has_first_insem = any(InsNumber == 1, na.rm = TRUE),

      # First insemination date where InsNumber == 1 (if present)
      first_insem_date = case_when(
        has_first_insem ~ suppressWarnings(min(InsDate[InsNumber == 1], na.rm = TRUE)),
        TRUE            ~ as.Date(NA)
      ),

      # Calving date for the lactation (min handles duplicates/NA)
      calving_date = suppressWarnings(min(LacCalvingDate, na.rm = TRUE)),

      # Difference in days (guard if any piece is missing/infinite)
      calving_to_insem_days = case_when(
        !is.na(first_insem_date) & is.finite(as.numeric(calving_date)) ~
          as.numeric(first_insem_date - calving_date),
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(-has_first_insem)
)


assign(paste0(farm_prefix, "_lactation_metrics"),
       get(paste0(farm_prefix, "_lactation_metrics")) %>%
         left_join(get(paste0(farm_prefix, "_calving_to_insem")), by = c("AniLifeNumber", "LacId"))
)


# Number of failed pregnancies per lactation cycle
assign(paste0(farm_prefix, "_failed_pregnancies"),
       get(paste0(farm_prefix, "_insem_lac_preg")) %>%
         group_by(LacId) %>%
         summarise(
           n_failed_pregnancies = sum(successful_pregnancy == FALSE,
                                      na.rm = TRUE),
           .groups = "drop"
         )
)

assign(paste0(farm_prefix, "_lactation_metrics"),
       get(paste0(farm_prefix, "_lactation_metrics")) %>%
         left_join(get(paste0(farm_prefix, "_failed_pregnancies")), by = "LacId") %>%
         mutate(n_failed_pregnancies = replace_na(n_failed_pregnancies, 0))
)

# Calculate the calving interval (time between one calving and the next)
assign(paste0(farm_prefix, "_calving_interval"),
       get(paste0(farm_prefix, "_lactation_metrics")) %>%
         group_by(AniLifeNumber) %>%
         arrange(AniLifeNumber, LacCalvingDate) %>%
         mutate(
           calving_interval_days = as.numeric(difftime(lead(LacCalvingDate), LacCalvingDate, units = "days")),
           calving_interval_reason = case_when(
             is.na(calving_interval_days) & still_milking ~ "still milking",
             is.na(calving_interval_days) ~ "no subsequent calving",
             TRUE ~ "recorded"
           )
         ) %>%
         ungroup() %>%
         select(AniLifeNumber, LacCalvingDate, calving_interval_days, calving_interval_reason)
)


assign(paste0(farm_prefix, "_lactation_metrics"),
       get(paste0(farm_prefix, "_lactation_metrics")) %>%
         left_join(get(paste0(farm_prefix, "_calving_interval")), by = c("AniLifeNumber", "LacCalvingDate"))
)

# Cache and export
cache(paste0(farm_prefix, "_lactation_metrics"))
write.csv(get(paste0(farm_prefix, "_lactation_metrics")), here("data", paste0(farm_prefix, "_lactation_metrics.csv")))