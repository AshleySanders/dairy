# ------------------------------------------------------------------------------
# Script: 04_repro_analysis.R
# Purpose: Analyze insemination outcomes and calving spacing to summarize
#          reproduction performance at the cow level.
#
# Scope / What this script does
#   1) Failed inseminations per cow
#      - Count total vs failed AIs; compute per-cow success_rate.
#      - Histogram and summary of success rates.
#   2) Reproductive efficiency
#      - For each lactation, flag whether the FIRST AI conceived.
#      - Summarize per cow: lactations_with_insem, conceptions_at_first_ai,
#        repro_efficiency (= first-AI conception proportion).
#      - Visualize distribution of repro_efficiency.
#   3) Calving intervals
#      - Derive calving_interval_days/months from consecutive calving dates.
#      - Summarize per cow: n_calvings, avg calving interval.
#
# Inputs (objects expected in the environment)
#   - insem_lac_preg  # per-AI records joined to lactations (includes InsDate,
#                     # LacNumber, LacCalvingDate, PreLacId, AniLifeNumber)
#
# Key Outputs (produced interactively / to console)
#   - failed_insem_per_cow (data.frame): totals, failures, success_rate
#   - repro_efficiency (data.frame): first-AI conception performance per cow
#   - calving_intervals, calving_summary (data.frames): spacing between calvings
#   - Plots: histogram of success_rate and repro_efficiency
#
# Dependencies
#   - Packages: dplyr, ggplot2, here (optional for project root)
#
# Assumptions / Filters
#   - Failed AI defined as NA PreLacId.
#   - First AI per lactation chosen by earliest InsDate (ties excluded).
#   - Calving intervals computed from distinct calving dates per cow; months
#     approximated using 30.44 days.
#
# How to run
#   - Ensure insem_lac_preg is loaded (see data preparation scripts).
#   - Source this script after constructing insem_lac_preg.
#
# Repro tips
#   - Save outputs for downstream use:
#       saveRDS(list(failed_insem_per_cow, repro_efficiency,
#                    calving_intervals, calving_summary),
#               "data/repro_analysis_outputs.rds")
# ------------------------------------------------------------------------------

# This script uses the insem_lac_preg dataset from the data folder

library(dplyr)
library(here)
here()

# Failed inseminations per cow
failed_insem_per_cow <- insem_lac_preg %>%
  group_by(AniLifeNumber) %>%
  summarise(
    total_inseminations = n(),
    failed_inseminations = sum(is.na(PreLacId)),
    success_rate = 1 - failed_inseminations / total_inseminations,
    .groups = "drop"
  )

hist(failed_insem_per_cow$success_rate)

# View the summary of failed inseminations per cow
summary(failed_insem_per_cow)

# Examine reproductive efficiency
repro_efficiency <- insem_lac_preg %>%
  # sort inseminations chronologically
  arrange(AniLifeNumber, LacNumber, InsDate) %>%

  # Identify first insemination per cow per lactation
  group_by(AniLifeNumber, LacNumber) %>%
  slice_min(order_by = InsDate, with_ties = FALSE) %>%

  # Mark if it resulted in pregnancy
  mutate(conceived_at_first_ai = !is.na(PreLacId)) %>%

  # Summarize at the cow level
  group_by(AniLifeNumber) %>%
  summarise(
    lactations_with_insem = n(),
    conceptions_at_first_ai = sum(conceived_at_first_ai),
    repro_efficiency = conceptions_at_first_ai / lactations_with_insem,
    .groups = "drop"
  )

# Output:
# lactations_with_insem: Number of lactations with at least one insemination
# conceptions_at_first_ai: Number of lactations where the first insemination resulted in pregnancy
# repro_efficiency: Proportion of lactations where the first insemination resulted in pregnancy

# View the reproductive efficiency summary
View(repro_efficiency)

# Visualize the reproductive efficiency
library(ggplot2)
ggplot(repro_efficiency, aes(x = repro_efficiency)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Reproductive Efficiency Distribution",
       x = "Reproductive Efficiency (Proportion of First Inseminations Resulting in Pregnancy)",
       y = "Count") +
  theme_minimal()


# Analyze calving intervals
calving_intervals <- insem_lac_preg %>%
  select(AniLifeNumber, LacNumber, LacCalvingDate) %>%
  distinct() %>%
  arrange(AniLifeNumber, LacCalvingDate) %>%
  group_by(AniLifeNumber) %>%
  mutate(
    calving_interval_days = as.numeric(LacCalvingDate - lag(LacCalvingDate)),
    calving_interval_months = calving_interval_days / 30.44  # Average days per month
  ) %>%
  ungroup()

# Summarize calving intervals
calving_summary <- calving_intervals %>%
  group_by(AniLifeNumber) %>%
  summarise(
    n_calvings = n(),
    avg_interval_days = mean(calving_interval_days, na.rm = TRUE),
    avg_interval_months = mean(calving_interval_months, na.rm = TRUE),
    .groups = "drop"
  )

View(calving_summary)
