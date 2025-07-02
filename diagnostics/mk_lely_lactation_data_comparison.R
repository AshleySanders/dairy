# Compare Mil'Klic data with lactation data calculated from Lely Milk Day Production table.

# --- Ensure AniLifeNumber from Lely data is cleaned so it matches national_number from the Supabase Mil'Klic tables. ---
lac_calving_calculated <- lac_calving_calculated %>%
  mutate(AniLifeNumber = str_replace_all(str_trim(as.character(AniLifeNumber)), "\\s+", ""))

mk_animals_lactations <- mk_animals_lactations %>%
  mutate(national_number = str_replace_all(str_trim(as.character(national_number)), "\\s+", ""))


# Check a few values to ensure formats match
head(lac_calving_calculated$AniLifeNumber)
head(mk_animals_lactations$national_number)

# Create & evaluate intersection of cows who are common to both datasets
common_ids <- intersect(lac_calving_calculated$AniLifeNumber, mk_animals_lactations$national_number)

# Determine number of cows in common between the two datasets
length(common_ids)

# Double check against Lely milking data
length(unique(lac_calving_calculated$AniLifeNumber))

# Double check number of cows for this customer_id in Mil'Klic data


# Restrict comparison to cows common to both datasets
lac_common <- lac_calving_calculated %>%
  filter(AniLifeNumber %in% common_ids)

milklic_common <- mk_animals_lactations %>%
  filter(national_number %in% common_ids)

# --- Select key columns to compare ---
lely_subset <- lac_common %>%
  select(AniLifeNumber, milk_production_start_date, lac_number_calculated, still_milking, total_milk_prod, lactation_duration)

milklic_subset <- milklic_common %>%
  select(national_number, lactation_start_date, lactation_number, lactation_status, lactation_total_milk_production_kg, lactation_duration)

# --- Anti-join to identify mismatches ---

# Lely lactation cycles not in Mil'Klic
mismatches_lely_only <- anti_join(lely_subset, milklic_subset,
                                  by = c("AniLifeNumber" = "national_number",
                                         "lac_number_calculated" = "lactation_number"))

View(mismatches_lely_only)

# Mil'Klic lactation cycles that are not in Lely-derived dataset
mismatches_milklic_only <- anti_join(milklic_subset, lely_subset,
                                     by = c("national_number" = "AniLifeNumber",
                                            "lactation_number" = "lac_number_calculated"))

valid_mismatches_milklic_only <- mismatches_milklic_only %>%
  filter(lactation_number > 0, lactation_start_date < as.Date("2024-09-19"))

View(valid_mismatches_milklic_only)

# Compare start dates with a fuzzy join
lac_start_comparison <- inner_join(
  lely_subset,
  milklic_subset,
  by = c("AniLifeNumber" = "national_number",
         "lac_number_calculated" = "lactation_number")
) %>%
  mutate(
    date_diff = as.numeric(milk_production_start_date - lactation_start_date)
  ) %>%
  filter(abs(date_diff) > 10)


View(lac_start_comparison)

# Check differences in the total number of lactations per cow across the two dataframes

lely_counts <- lely_subset %>%
  group_by(AniLifeNumber) %>%
  summarise(n_lely = n())

milklic_counts <- milklic_subset %>%
  group_by(national_number) %>%
  filter(lactation_number > 0, lactation_start_date < as.Date("2024-09-19")) %>%
  summarise(n_milklic = n())

compare_counts <- lely_counts %>%
  full_join(milklic_counts, by = c("AniLifeNumber" = "national_number")) %>%
  mutate(diff = n_lely - n_milklic) %>%
  filter(diff != 0)

View(compare_counts)
