# ------------------------------------------------------------------------------
# Script Name:    04_culling_decision_analysis.R
# Project:       Herd Management Strategy
#
# Purpose:        Model and analyze factors that predict dairy cow culling
#                 decisions using a Random Survival Forest (RSF) approach.
#
# Description:    This script:
#                 1. Loads lactation-level production, reproduction, and health
#                    event data for each cow.
#                 2. Engineers features required for survival analysis, including:
#                    - Time-to-exit (censoring for cows still in the herd)
#                    - Aggregated health events per lactation
#                 3. Imputes missing reproductive data (n_insem) for modeling.
#                 4. Fits an RSF model to estimate relative variable importance
#                    and risk of culling.
#                 5. Validates model performance with:
#                    - C-index (via ranger OOB error)
#                    - Kaplan–Meier curves by predicted risk group
#                 6. Profiles characteristics of high- vs. low-risk cows.
#                 7. Maps risk profiles back to management decisions
#                     (potential culling triggers).
#
# Inputs:
#   - cow_features:           Cow-level metadata, including birth and exit dates.
#   - lactation_metrics:      Lactation-cycle-level milk yield and reproduction data.
#   - dairy_health_problems:  Raw health event records with diagnosis descriptions.
#
# Outputs:
#   - Variable importance plot (ggplot)
#   - KM curves by RSF risk group
#   - Summary tables of group characteristics
#   - Boxplots and barplots of key driver variables by risk group
#   - Profiles of high-risk vs. low-risk cows for management review
#
# Dependencies:
#   Libraries: forcats, survival, ranger, car, ggplot2, tibble, dplyr, tidyr
#   Custom functions: clean_ani() – used for AniLifeNumber ID cleaning.
#
# Notes:
#   - Survival time is measured in months from AniBirthday to exit_date
#     (or current date if censored).
#   - Missing n_insem values are median-imputed for modeling consistency.
#   - Interaction term n_insem * n_failed_pregnancies is included in RSF model.
#   - RSF splitrule = "logrank" for survival outcomes; cannot handle NA predictors.
#   - Risk groups are quartiles of predicted risk scores (higher = more at risk).
#   - Management recommendations are illustrative; thresholds should be validated
#     with domain experts and economic analysis.
#
# Author:         Ashley Sanders
# Created:        2025-08-06
# Last Updated:   2025-08-08
# ------------------------------------------------------------------------------
library(forcats)
library(survival)
library(ranger)
library(car)
library(ggplot2)
library(tibble)
library(fastshap)
library(doParallel)
library(parallel)

# --- Feature Engineering ---
# Create censoring flag and time-to-exit in days from birthdate
cow_features <- cow_features %>%
  mutate(
    censored = is.na(exit_date),
    time_to_exit_days = as.numeric(difftime(exit_date, AniBirthday, units = "days"))
  )

# Standardize health problem data and create simplified diagnosis categories
dairy_health_problems <- dairy_health_problems %>%
  mutate(
    AniLifeNumber = clean_ani(AniLifeNumber),
    DiaDate = as.Date(DiaDate),
    diagnosis_category = case_when(
      grepl("mastitis", DisName, ignore.case = TRUE) ~ "mastitis",
      grepl("fertil", DisName, ignore.case = TRUE) ~ "fertility",
      grepl("cyst", DisName, ignore.case = TRUE) ~ "cyst",
      grepl("lamen", DisName, ignore.case = TRUE) ~ "lameness",
      TRUE ~ "other"
    )
  )

# Summarize health events per lactation cycle
health_per_lactation <- dairy_health_problems %>%
  inner_join(lactation_metrics %>%
               select(AniLifeNumber, LacCalvingDate, dry_off_date, RemLactation_LacNumber),
             by = "AniLifeNumber", relationship = "many-to-many") %>%
  mutate(
    DiaDate = as.Date(DiaDate),
    LacCalvingDate = as.Date(LacCalvingDate),
    dry_off_date = as.Date(dry_off_date)
  ) %>%
  filter(DiaDate >= LacCalvingDate & DiaDate <= dry_off_date) %>%
  group_by(AniLifeNumber, RemLactation_LacNumber) %>%
  summarise(
    n_health_events = n(),
    mastitis = any(diagnosis_category == "mastitis"),
    fertility_issues = any(diagnosis_category == "fertility"),
    lameness = any(diagnosis_category == "lameness"),
    .groups = "drop"
  )

# Join data from lactation_metrics, cow_features, and health_per_lactation
cow_survival <- lactation_metrics %>%
  left_join(
    cow_features %>%
      select(AniLifeNumber, age_at_exit, censored),
    by = "AniLifeNumber"
  ) %>%
  left_join(
    health_per_lactation, by = c("AniLifeNumber", "RemLactation_LacNumber")
  ) %>%
  mutate(across(c(mastitis, fertility_issues, lameness), ~replace_na(., FALSE)))

# Fill in missing age_at_exit for censored cows using current date
today <- as.Date("2025-08-06")  # or use Sys.Date()

cow_survival <- cow_survival %>%
  mutate(
    age_at_exit = ifelse(
      !is.na(exit_date),
      interval(AniBirthday, exit_date) %/% months(1),
      interval(AniBirthday, today) %/% months(1)
    )
  )

# Check for missing values in predictor variables
sapply(cow_survival[, c("total_milk_production", "n_insem",
                        "n_failed_pregnancies", "age_at_calving")],
       function(x) sum(is.na(x)))

# Diagnose and explore rows with missing n_insem values
sum(is.na(lactation_metrics$n_insem))
missing_insem <- lactation_metrics %>% filter(is.na(n_insem))
table(missing_insem$first_pregnancy)
nrow(missing_insem %>% filter(first_pregnancy == FALSE, LacCalvingDate < as.Date("2017-10-01")))
table(missing_insem$last_lactation)

# Impute NAs for n_insem using median value for modeling
cow_survival <- cow_survival %>%
  mutate(
    n_insem = coalesce(n_insem, median(n_insem, na.rm = TRUE))
  )


# --- Fit random survival forest model ---
cow_survival <- cow_survival %>%
  mutate(insem_failed_interaction = n_insem * n_failed_pregnancies)

# Create a survival object (event = 1 if the cow exited)
surv_obj <- Surv(time = cow_survival$age_at_exit, event = cow_survival$censored == FALSE)

# Fit RSF model with main predictors and one interaction term
rsf_model <- ranger(
  formula = surv_obj ~ total_milk_production + n_insem + n_failed_pregnancies +
    insem_failed_interaction + age_at_calving + mastitis + fertility_issues + lameness,
  data = cow_survival,
  mtry = 3,
  importance = "permutation",
  num.trees = 3000,
  splitrule = "logrank",
  seed = 42
)

# Inspect variable importance and prediction error
rsf_model$variable.importance
rsf_model$prediction.error

# --- Visualize variable importance ---
var_imp_df <- enframe(rsf_model$variable.importance,
                      name = "Variable",
                      value = "Importance"
                      ) %>%
  arrange(desc(Importance))

ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Relative Importance of Predictors",
    x = "Predictor Variable",
    y = "Variable Importance (Permutation)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )


# --- Validation with Kaplan-Meier Curves by Risk Group ---

# Survival curve matrix: each row is a cow, each column is timepoint
rsf_surv_curves <- rsf_model$survival

# Compute mean survival probability per cow across all timepoints, then risk score
mean_survival <- rowMeans(rsf_surv_curves)
cow_survival$risk_score <- 1 - mean_survival  # higher = more at risk

# Assign quartile-based risk groups
cow_survival_clean <- cow_survival %>%
  mutate(risk_group = ntile(risk_score, 4))  # quartiles

# Join risk score and risk group to cow_features
cow_features <- cow_features %>%
  left_join(cow_survival_clean %>%
              select(AniLifeNumber, risk_score, risk_group),
            by = "AniLifeNumber")

# Fit Kaplan-Meier survival curves by RSF risk group
km_fit <- survfit(Surv(age_at_exit, !censored) ~ risk_group, data = cow_survival_clean)

ggsurvplot(km_fit, data = cow_survival_clean, pval = TRUE,
           risk.table = TRUE, palette = "Dark2",
           title = "Survival by RSF-predicted Risk Group")


# --- Examine characteristics of each risk group ---
cow_survival_clean %>%
  group_by(risk_group) %>%
  summarise(
    n = n(),
    avg_milk = mean(total_milk_production, na.rm = TRUE),
    avg_insem = mean(n_insem, na.rm = TRUE),
    avg_failed_preg = mean(n_failed_pregnancies, na.rm = TRUE),
    avg_age_calving = mean(age_at_calving, na.rm = TRUE),
    pct_mastitis = mean(mastitis, na.rm = TRUE),
    pct_fert_issues = mean(fertility_issues, na.rm = TRUE),
    pct_lameness = mean(lameness, na.rm = TRUE)
  )

# --- Visualize key drivers by risk group ---

# Boxplot for milk production by risk group
ggplot(cow_survival_clean, aes(x = as.factor(risk_group), y = total_milk_production)) +
  geom_boxplot() +
  labs(title = "Milk Production by RSF Risk Group", x = "Risk Group", y = "Total Milk (L)") +
  theme_classic()

# Barplot for prevalence of health issues by risk group
cow_survival_clean %>%
  group_by(risk_group) %>%
  summarise(
    mastitis = mean(mastitis),
    fertility = mean(fertility_issues),
    lameness = mean(lameness)
  ) %>%
  pivot_longer(cols = c(mastitis, fertility, lameness), names_to = "Issue", values_to = "Rate") %>%
  ggplot(aes(x = as.factor(risk_group), y = Rate, fill = Issue)) +
  geom_col(position = "dodge") +
  labs(title = "Health Issue Rate by Risk Group", x = "Risk Group", y = "Proportion") +
  theme_classic()


# --- Map RSF risk scores back to management decisions ---

# Profile high-risk cows (Group 4)

cow_survival_clean %>%
  filter(risk_group == 4) %>%
  summarise(
    avg_milk = mean(total_milk_production, na.rm = TRUE),
    avg_insem = mean(n_insem, na.rm = TRUE),
    avg_failed_preg = mean(n_failed_pregnancies, na.rm = TRUE),
    pct_mastitis = mean(mastitis, na.rm = TRUE),
    pct_lameness = mean(lameness, na.rm = TRUE),
    avg_age_at_calving = mean(age_at_calving, na.rm = TRUE)
  )

# Profile lowest-risk cows (Group 1)
cow_survival_clean %>%
  filter(risk_group == 1) %>%
  summarise(
    avg_milk = mean(total_milk_production, na.rm = TRUE),
    avg_insem = mean(n_insem, na.rm = TRUE),
    avg_failed_preg = mean(n_failed_pregnancies, na.rm = TRUE),
    pct_mastitis = mean(mastitis, na.rm = TRUE),
    pct_lameness = mean(lameness, na.rm = TRUE),
    avg_age_at_calving = mean(age_at_calving, na.rm = TRUE)
  )

# --- Extract SHAP values from the RSF to identify which variables increase each individual cow's risk score ---

# Create a smaller model to calculate SHAP values
rsf_model <- ranger(
  formula = surv_obj ~ total_milk_production + n_insem + n_failed_pregnancies +
    insem_failed_interaction + age_at_calving + mastitis + fertility_issues + lameness,
  data = cow_survival,
  mtry = 3,
  importance = "permutation",
  num.trees = 500,
  splitrule = "logrank",
  seed = 42
)

# Compute SHAP values for all cows

# pick a reasonble number of workers
n_cores <- min( max(1, parallel::detectCores() -1), 12 )
cl <- makeCluster(n_cores)
registerDoParallel(cl)
set.seed(42) # reproducibility

# Define predictor variable names (same as in the RSF model)
ids <- cow_survival$AniLifeNumber
predictors <- c("total_milk_production", "n_insem", "n_failed_pregnancies", "insem_failed_interaction", "age_at_calving", "mastitis", "fertility_issues", "lameness")

X <- cow_survival %>% select(all_of(predictors))

# quick sanity checks
stopifnot(setequal(names(X), predictors))
stopifnot(nrow(X) == length(ids))

# wrapper: returns numeric risk score (1-mean survival)
risk_fun <- function(object, newdata) {
  pred <- predict(object, data = newdata)$survival
  1 - rowMeans(pred)
}

# keep nsim moderate first; increase later if stable
shap_values <- fastshap::explain(
  object        = rsf_model,
  feature_names = colnames(X),
  pred_wrapper  = risk_fun,
  X             = X,
  nsim          = 50
)

# always stop the cluster
parallel::stopCluster(cl)  # stop cluster

# For each cow, find the variable with the largest positive SHAP value

# Convert to tidy form and attach IDs
shap_df <- as.data.frame(unclass(shap_values)) %>%  # drop "explain" class
  setNames(colnames(X)) %>%
  mutate(AniLifeNumber = ids) %>%
  as_tibble()

# Top driver per cow (handle NAs safely)
top_driver <- shap_df %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "variable", values_to = "shap_value") %>%
  mutate(shap_value = replace_na(as.numeric(shap_value), 0)) %>%
  group_by(AniLifeNumber) %>%
  slice_max(order_by = shap_value, n = 1, with_ties = FALSE) %>%
  ungroup()

# Map to a readable reason and join back to cow_features
reason_map <- c(
  "total_milk_production" = "Low milk yield",
  "n_insem"               = "High insemination count",
  "n_failed_pregnancies"  = "Reproduction failure",
  "age_at_calving"        = "Age at calving",
  "mastitis"              = "Chronic mastitis",
  "fertility_issues"      = "Fertility issues",
  "lameness"              = "Lameness"
)

top_driver <- top_driver %>%
  mutate(est_reason_for_culling = unname(reason_map[variable]))

cow_features <- cow_features %>%
  left_join(top_driver %>% select(AniLifeNumber, est_reason_for_culling),
            by = "AniLifeNumber")
