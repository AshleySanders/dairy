# ------------------------------------------------------------------------------
# Script Name:     diagnostics/multiple_entries_exits.R
# Author:          Ashley Sanders
# Date Created:    2025-07-02
# Last Modified:   2025-07-02
#
# Description:
# This diagnostic script explores animal entry and exit patterns in the
# `animals_history` dataset from Supabase. It checks for cows with multiple
# entry and exit events, which may indicate inter-farm transfers or re-entries.
# The script flags cows with multiple entries, summarizes entry/exit code
# combinations, and examines cows with two "A" (arrival) entries.
#
# Input Data:
# - animals_history: Table of animal movement and lifecycle events
#
# Output:
# - multiple_entries_exits.csv: Cleaned table of cows with multiple distinct
#   entry/exit pairs
#
# Key Operations:
# - Counts cows with multiple unique entry and exit dates
# - Summarizes entry/exit events by animal
# - Performs contingency analysis on entry/exit code associations
# - Identifies cows with exactly two "A" entries and inspects their exit codes
#
# Dependencies:
# - Packages: dplyr, here
# - Environment: Assumes `animals_history` is already loaded in memory
#
# Notes:
# - This is a diagnostics script; outputs are not integrated into modeling
# - Used to support interpretation of missing data and data integration across farms
# ------------------------------------------------------------------------------

multiple_entries <- animals_history %>%
  filter(!is.na(entry_date)) %>%       # ensure we’re only counting real entries
  group_by(animal) %>%
  summarise(entry_count = n_distinct(entry_date), .groups = "drop") %>%
  filter(entry_count > 1)

print(multiple_entries)

max(multiple_entries$entry_count)

# Get entry_code and entry_date for each of those animals (distinct only)
entry_events <- animals_history %>%
  filter(animal %in% multiple_entries$animal, !is.na(entry_date)) %>%
  select(animal, entry_date, entry_code) %>%
  distinct() %>%
  arrange(animal, entry_date)

# View result
entry_events

# Examine exit codes
multiple_exits <- animals_history %>%
  filter(!is.na(exit_date)) %>%       # ensure we’re only counting real entries
  group_by(animal) %>%
  summarise(exit_count = n_distinct(exit_date), .groups = "drop") %>%
  filter(exit_count > 1)

exit_events <- animals_history %>%
  filter(animal %in% multiple_exits$animal, !is.na(exit_date)) %>%
  select(animal, exit_date, exit_code) %>%
  distinct() %>%
  arrange(animal, exit_date)

# View result
exit_events

table(exit_events$exit_code)

multiple_entries_exits <- animals_history %>%
  filter(
    !is.na(entry_date),
    !is.na(exit_date),
    animal %in% multiple_entries$animal,
    animal %in% multiple_exits$animal
    ) %>%
  select(animal, entry_date, entry_code, exit_date, exit_code) %>%
  distinct() %>%
  arrange(animal, entry_date)

View(multiple_entries_exits)

# Examine potential correlation between entry and exit codes
contingency_entry_exit_codes <- table(multiple_entries_exits$entry_code, multiple_entries_exits$exit_code)

chisq.test(contingency_entry_exit_codes)  # inconclusive
fisher.test(contingency_entry_exit_codes) # significant

cows_AA <- multiple_entries_exits %>%
  filter(entry_code == "A") %>%
  group_by(animal) %>%
  summarise(num_A_entries = n(), .groups = "drop") %>%
  filter(num_A_entries == 2)

cows_AA

exit_AA <- multiple_entries_exits %>%
  filter(animal %in% cows_AA$animal) %>%
  select(animal, exit_date, exit_code) %>%
  distinct()

exit_AA

table(exit_AA$exit_code) # view the exit codes for cows with two "A" entry codes.

write.csv(multiple_entries_exits, here("data", "multiple_entries_exits.csv"))

# --- Identify animals with more than one unique entry date and flag them in animals_history ---

animals_history <- animals_history %>%
  mutate(has_multiple_entries = animal %in% multiple_entries$animal)

# double check results
animals_history %>%
  group_by(animal) %>%
  filter(has_multiple_entries == TRUE) %>%
  summarise(true_multiple_entries = n(), .groups = "drop") %>%
  distinct(animal)

# --- Add this information into cow_features ---
cow_multi_entry_flag <- animals_history %>%
  filter(has_multiple_entries == TRUE) %>%
  distinct(animal)

cow_features <- cow_features %>%
  mutate(has_multiple_entries = AniLifeNumber %in% cow_multi_entry_flag$animal)


