# ------------------------------------------------------------------------------
# Script: 03_health_analysis.R
# Author: Ashley Sanders
# Date:   2025-09-15
#
# Purpose:
#   Analyze health problems among cows in Farm 1 (Farm 5 excluded due to
#   unreliable statistics). The analysis investigates whether older cows
#   experience more frequent and costly health issues, particularly related to
#   insemination and pregnancy.
#
# Overview:
#   1. Data preparation:
#      - Join diagnosis data with cow features to calculate age at diagnosis.
#      - Bin ages into categories (<2, 2, 3, 4, 5, 6, 7+ years).
#      - Create summary tables and visualizations of age at diagnosis.
#
#   2. Statistical tests:
#      - Chi-square tests for distribution of diagnoses by age bins.
#      - Mixed-effects logistic regression models (weighted & unweighted) to
#        estimate probability of diagnosis as a function of age while
#        accounting for repeated diagnoses per cow.
#
#   3. Longitudinal (panel) logistic regression:
#      - Build cow-year panel data up to exit/censor date (2024-09-19 for
#        cows still in herd).
#      - Fit generalized linear mixed-effects model with spline terms for age.
#      - Estimate and visualize predicted probabilities of diagnosis by age.
#
#   4. Outputs:
#      - Summary statistics and diagnostic plots (histograms, boxplots).
#      - Model summaries for weighted, unweighted, and panel logistic models.
#      - Predicted probabilities of diagnosis for ages 2–9 years with
#        confidence intervals.
#
# Notes:
#   - Uses `lme4`, `rstatix`, `ggpubr`, `ggeffects`, `splines`, and `ggplot2`.
#   - Main outcome variable: diag_event (1 = diagnosis occurred, 0 = no diagnosis).
#   - Random effects included for AniLifeNumber to account for repeated measures.
# ------------------------------------------------------------------------------

# Use Farm1 since we don't have reliable statistics from Farm5.

# Do older cows have more health problems?

fm1_dairy_health_problems <- dairy_health_problems

colnames(fm1_dairy_health_problems)

# Add a column for AniBirthday and a column for age_at_diag
fm1_dairy_health_problems <- fm1_dairy_health_problems %>%
  left_join(fm1_cow_features %>%
              select(AniBirthday, AniLifeNumber),
            by = "AniLifeNumber")

fm1_dairy_health_problems <- fm1_dairy_health_problems %>%
  mutate(age_at_diag = interval(as.Date(AniBirthday), as.Date(DiaDate)) %/% months(1))

fm1_dairy_health_problems <- fm1_dairy_health_problems %>%
  mutate(age_at_diag_yrs = case_when(
    age_at_diag < 24 ~ "<2",
    age_at_diag >= 24 & age_at_diag < 36 ~ "2",
    age_at_diag >= 36 & age_at_diag < 48 ~ "3",
    age_at_diag >= 48 & age_at_diag < 60 ~ "4",
    age_at_diag >= 60 & age_at_diag < 72 ~ "5",
    age_at_diag >= 72 & age_at_diag < 84 ~ "6",
    age_at_diag >=84 ~ "7+"
  ))

fm1_dairy_health_problems <- fm1_dairy_health_problems %>%
  mutate(cow_age_at_diag_bin = case_when(
    age_at_diag_yrs == "<2" ~ "young_diag",
    age_at_diag >=24 & age_at_diag <= 60 ~ "mature_diag",
    age_at_diag > 60 ~ "old_diag"
  ))

# Summary statistics
summary(fm1_dairy_health_problems$age_at_diag)

hist(fm1_dairy_health_problems$age_at_diag,
     main = "Cows' Age at Diagnosis",
     xlab = "Months",
     ylab = "Number of Cows")

boxplot(fm1_dairy_health_problems$age_at_diag,
        main = "Cows' Age at Diagnosis", horizontal = TRUE,
        xlab = "Months")

# Test for correlation between age and number of health problems

table(fm1_dairy_health_problems$cow_age_at_diag_bin)

age_tab <- table(fm1_dairy_health_problems$cow_age_at_diag_bin)
chisq.test(age_tab)   # Chi-square goodness-of-

# How does the probability of a diagnosis vary with age, while accounting for the fact that multiple diagnoses (or none) can come from the same cow?
fm1_cow_health <- fm1_cow_features %>%
  select(AniLifeNumber, AniBirthday, age_at_exit, n_health_problems)

# Diagnosis rows (events)
events <- fm1_dairy_health_problems %>%
  transmute(
    AniLifeNumber,
    AniBirthday,
    age_at_diag = age_at_diag,   # already in months
    diag_event = 1
  )

# Healthy cows (no health problems)
healthy <- fm1_cow_features %>%
  filter(n_health_problems == 0) %>%
  transmute(
    AniLifeNumber,
    AniBirthday,
    age_at_diag = age_at_exit,   # use exit age as censor
    diag_event = 0
  )

# Combine both
fm1_cow_health_logit <- bind_rows(events, healthy)

# Add weight to account for the fact that some cows have multiple diagnoses and healthy cows only have one row in the table.
fm1_cow_health_logit <- fm1_cow_health_logit %>%
  group_by(AniLifeNumber) %>%
  mutate(n_diag_rows = n()) %>%
  ungroup() %>%
  mutate(weight = 1 / n_diag_rows)

# Fit weighted mixed-effects logistic regression
m1 <- glmer(
  diag_event ~ age_at_diag + (1 | AniLifeNumber),
  data = fm1_cow_health_logit,
  family = binomial,
  weights = weight
)

summary(m1)

# Fit an unweighted mixed-effects logistic regressoin
m2 <- glmer(
  diag_event ~ age_at_diag + (1 | AniLifeNumber),
  data = fm1_cow_health_logit,
  family = binomial
)

summary(m2)

#-------------------------------------------------------------------------------
# Previous models didn't take age at exit into consideration. Switching to a panel (longitudinal) logistic regression model.

study_end_date <- as.Date("2024-09-19")

cows <- fm1_cow_features %>%
  mutate(
    AniBirthday = as.Date(AniBirthday),
    exit_date   = as.Date(exit_date),
    last_seen_date = if_else(!is.na(exit_date), exit_date, study_end_date),
    # age at censor in YEARS (integer, age 0..censor_age_yr)
    censor_age_yr = as.integer(floor(time_length(interval(AniBirthday, last_seen_date), "years")))
  ) %>%
  filter(!is.na(AniLifeNumber), !is.na(AniBirthday), !is.na(censor_age_yr), censor_age_yr >= 0) %>%
  select(AniLifeNumber, AniBirthday, last_seen_date, censor_age_yr)

# --- Diagnosis events aggregated to cow-year -----------------------------------
events_by_year <- fm1_dairy_health_problems %>%
  transmute(
    AniLifeNumber,
    age_year = floor(as.numeric(age_at_diag) / 12)
  ) %>%
  filter(!is.na(age_year), age_year >= 0) %>%
  count(AniLifeNumber, age_year, name = "diag_count") %>%
  mutate(diag_event = 1L)

# --- Expand to cow-year panel up to censor age ---------------------------------
panel <- cows %>%
  mutate(n_years = censor_age_yr + 1L) %>%                 # ages 0..censor_age_yr
  tidyr::uncount(n_years) %>%
  group_by(AniLifeNumber) %>%
  mutate(age_year = row_number() - 1L) %>%
  ungroup()

panel_logit <- panel %>%
  left_join(events_by_year, by = c("AniLifeNumber", "age_year")) %>%
  mutate(
    diag_event = coalesce(diag_event, 0L),
    diag_count = coalesce(diag_count, 0L)
  )

# (Optional) add covariates
panel_logit <- panel_logit %>%
  left_join(
    fm1_cow_features %>%
      select(AniLifeNumber, cohort, avg_total_milk, number_lactations),
    by = "AniLifeNumber"
  )

# library(lme4); library(splines)
m_panel <- glmer(diag_event ~ bs(age_year, df = 4) + (1 | AniLifeNumber),
                 data = panel_logit, family = binomial)
summary(m_panel)

#-------------------------------------------------------------------------------
# Plot fitted probabilities across ages

library(ggeffects)
pred <- ggpredict(m_panel, terms = "age_year [all]")

plot(pred) +
  labs(
    title = "Probability of at least one diagnosis per year by age",
    x = "Age (years)",
    y = "Predicted probability"
  ) +
  theme_classic()

#-------------------------------------------------------------------------------
# Identify predicted probabilities of health issues for cows between 2 and 9 years of age

# Define the ages of interest
ages_of_interest <- c(2, 3, 4, 5, 6, 7, 8, 9)

# Get predicted probabilities at those ages
pred_probs <- ggpredict(m_panel, terms = paste0("age_year [", paste(ages_of_interest, collapse = ","), "]"))

# Show results as a tidy table
pred_table <- as.data.frame(pred_probs) %>%
  select(x, predicted, conf.low, conf.high) %>%
  rename(
    age_year   = x,
    probability = predicted,
    lower_CI   = conf.low,
    upper_CI   = conf.high
  )

print(pred_table)

pred_probs <- ggpredict(
  m_panel,
  terms = paste0("age_year [", paste(ages_of_interest, collapse = ","), "]")
)

# Convert to data frame
pred_table <- as.data.frame(pred_probs)

# Plot only the 8 ages with CI bars
ggplot(pred_table, aes(x = x, y = predicted)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "steelblue") +
  scale_x_continuous(breaks = ages_of_interest) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Predicted Probability of Diagnosis by Age",
    x = "Age (years)",
    y = "Probability of ≥1 diagnosis in that year"
  ) +
  theme_classic(base_size = 14)