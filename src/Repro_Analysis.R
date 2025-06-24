# Analysis of reproduction data

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
