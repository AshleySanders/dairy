# ------------------------------------------------------------------------------
# Script: check_duplicates.R
# Purpose: Utility to check for duplicate AniLifeNumber keys in key farm tables.
# Notes:  Calls `check_dups()` on multiple datasets and reports counts of dupes.
# ------------------------------------------------------------------------------


check_dups <- function(df, name) {
  df %>%
    mutate(AniLifeNumber = clean_ani(AniLifeNumber)) %>%
    count(AniLifeNumber, name = "n") %>%
    filter(n > 1) %>%
    { cat(sprintf("%s: %d duplicated keys\n", name, nrow(.))); . }
}

check_dups(cow_lactation_summary, "cow_lactation_summary")  # should be 0
check_dups(endmilk_to_exit,      "endmilk_to_exit")
check_dups(exit_age,             "exit_age")
check_dups(cow_insem_summary,    "cow_insem_summary")
check_dups(cow_age_at_first_insem,          "cow_age_at_first_insem")
check_dups(cow_age_first_success_insem,     "cow_age_first_success_insem")
check_dups(chs,                  "chs")
check_dups(asl,                  "asl")
