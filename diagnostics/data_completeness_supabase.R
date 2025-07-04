# This script relies on datasets in R created from the Supabase SQL tables.

# animals_history
# Q: Are there multiple entry/exit dates for a subset of cows in this table, as there should be since some farms loan their cows to other farms?

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