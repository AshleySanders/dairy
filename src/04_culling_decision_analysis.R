# Culling analysis - use survival model to determine which factors are most predictive of culling
# Objective: model the time to cow exit using lactation-level features, health event history, and survival outcomes in a format compatible with randomFOrestSRC::rfsrc()

library(janitor)
library(forcats)
library(survival)
library(survminer)
library(ranger)
library(broom)
library(car)
library(ggplot2)

# Data:
# - cow_features. Note: age_at_exit is calculated in months.
# - lactation_metrics
# - dairy_health_problems

# --- Feature Engineering ---
cow_features <- cow_features %>%
  mutate(
    censored = is.na(exit_date),
    time_to_exit_days = as.numeric(difftime(exit_date, AniBirthday, units = "days"))
  )

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

# summarize health events per lactation cycle
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

# Ensure that there are no NAs in age_at_exit
today <- as.Date("2025-08-06")  # or use Sys.Date()

cow_survival <- cow_survival %>%
  mutate(
    age_at_exit = ifelse(
      !is.na(exit_date),
      interval(AniBirthday, exit_date) %/% months(1),
      interval(AniBirthday, today) %/% months(1)
    )
  )

# Ensure no NAs in predictor variables
sapply(cow_survival[, c("total_milk_production", "n_insem",
                        "n_failed_pregnancies", "age_at_calving")],
       function(x) sum(is.na(x)))

# Diagnose problems with NAs in n_insem
sum(is.na(lactation_metrics$n_insem))
missing_insem <- lactation_metrics %>% filter(is.na(n_insem))
table(missing_insem$first_pregnancy)
nrow(missing_insem %>% filter(first_pregnancy == FALSE, LacCalvingDate < as.Date("2017-10-01")))
table(missing_insem$last_lactation)

# Impute NAs for n_insem using median for modeling

cow_survival <- cow_survival %>%
  mutate(
    n_insem = coalesce(n_insem, median(n_insem, na.rm = TRUE))
  )


# --- Fit random survival forest model ---
# Create a survival object (event = 1 if the cow exited)
surv_obj <- Surv(time = cow_survival$age_at_exit, event = cow_survival$censored == FALSE)

# Fit the RSF model
rsf_model <- ranger(
  formula = surv_obj ~ total_milk_production + n_insem * n_failed_pregnancies +
    age_at_calving + mastitis + fertility_issues + lameness,
  data = cow_survival,
  mtry = 3,
  importance = "permutation",
  num.trees = 3000,
  splitrule = "logrank",
  seed = 42
)

# View variable importance
rsf_model$variable.importance
rsf_model$prediction.error

# Validation with Kaplan-Meier Curves by Risk Group
# Survival curve matrix: each row is a cow, each column is timepoint
rsf_surv_curves <- rsf_model$survival

# Mean predicted survival at last time point = risk proxy
mean_survival <- rowMeans(rsf_surv_curves)
cow_survival$risk_score <- 1 - mean_survival  # higher = more at risk

cow_survival_clean <- cow_survival %>%
  mutate(risk_group = ntile(risk_score, 4))  # quartiles

km_fit <- survfit(Surv(age_at_exit, !censored) ~ risk_group, data = cow_survival_clean)

ggsurvplot(km_fit, data = cow_survival_clean, pval = TRUE,
           risk.table = TRUE, palette = "Dark2",
           title = "Survival by RSF-predicted Risk Group")

# Examine the characteristics of each risk group
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

# Visualize key drivers by risk group
ggplot(cow_survival_clean, aes(x = as.factor(risk_group), y = total_milk_production)) +
  geom_boxplot() +
  labs(title = "Milk Production by RSF Risk Group", x = "Risk Group", y = "Total Milk (L)") +
  theme_classic()

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


# Check Against Cox Proportional Hazards Model, which uses a time-based method
library(survminer)

cox_model <- coxph(Surv(age_at_exit, !censored) ~
                     total_milk_production + n_insem + n_failed_insem +
                     n_failed_pregnancies + age_at_calving +
                     mastitis + fertility_issues + lameness,
                   data = cow_survival)

summary(cox_model)

ggforest(cox_model, data = cow_survival)