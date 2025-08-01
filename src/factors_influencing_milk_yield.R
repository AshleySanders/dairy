# Factors that influence milk production
# - Age at calving
# - Lactation duration
# - Dry-off interval

library(car)
library(FSA)
library(rstatix)
library(splines)
library(pwr)

# --- Age at Calving ---
# 1. Split ages of cows into groups
lactation_metrics <- lactation_metrics %>%
  mutate(
    age_at_calving_yrs = case_when(
      between(age_at_calving, 24, 35)  ~ "2 years",
      between(age_at_calving, 36, 47)  ~ "3 years",
      between(age_at_calving, 48, 59)  ~ "4 years",
      between(age_at_calving, 60, 71)  ~ "5 years",
      between(age_at_calving, 72, 83)  ~ "6 years",
      between(age_at_calving, 84, 95)  ~ "7 years",
      between(age_at_calving, 96,107)  ~ "8 years",
      between(age_at_calving,108,119)  ~ "9 years",
      between(age_at_calving,120,131)  ~ "10 years",
      TRUE                             ~ NA_character_
    ),
    age_at_calving_yrs = factor(
      age_at_calving_yrs,
      levels = c("2 years","3 years","4 years","5 years",
                 "6 years","7 years","8 years","9 years","10 years"),
      ordered = TRUE
    )
  )

# --- Check assumptions to determine appropriate test type ---
# Number of observations in each group
table(lactation_metrics$age_at_calving_yrs)

# Means of milk production for each age group
milk_means <- tapply(lactation_metrics$total_milk_production, INDEX = lactation_metrics$age_at_calving_yrs, FUN = mean)

milk_means # View means by group

# Check the equality of variance
# Calculate the standard deviations for each of the groups
milk_sds <- tapply(lactation_metrics$total_milk_production, INDEX = lactation_metrics$age_at_calving_yrs, FUN = sd)

# Calculate the quotient of max & min SDs. If < 2, assume equality of variance
max(milk_sds)/min(milk_sds)

# Check normality assumption
milk_meancen <- lactation_metrics$total_milk_production - milk_means[as.numeric(lactation_metrics$age_at_calving_yrs)] # Calculated group-wise mean-centered values (residuals)

qqnorm(milk_meancen, main = "Normal QQ plot of residuals")

qqline(milk_meancen)

# Fit 1-way ANOVA
fit <- aov(total_milk_production ~ age_at_calving_yrs, data = lactation_metrics)
summary(fit) # View results of ANOVA

# Different assumption tests
# Normality of residuals
shapiro.test(residuals(fit))

# Homogeneity of variance
leveneTest(total_milk_production ~ age_at_calving_yrs, data = lactation_metrics)

# Since the data violates all of the assumptions and is unbalanced, we'll use a Kruskal-Wallis test

kruskal.test(total_milk_production ~ age_at_calving_yrs, data = lactation_metrics)

# View differences
ggplot(lactation_metrics, aes(x=age_at_calving_yrs, y = total_milk_production)) +
  geom_boxplot() +
  labs(title = "Milk Production Across Age Groups",
       x = "Age at Calving",
       y = "Total Milk Production (L)") +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Dunn's posthoc test
df <- lactation_metrics %>%
  filter(!is.na(age_at_calving_yrs)) %>%
  mutate(age_at_calving_yrs = factor(age_at_calving_yrs))

df %>%
  dunn_test(
    total_milk_production ~ age_at_calving_yrs,
    p.adjust.method = "bonferroni"
    ) %>%
  print(n = 36)

# --- Examining effect of lactation duration. ---
# Hypothesis: longer lactation intervals produce higher milk yields

lm_lac_dur <- lm(total_milk_production ~ lactation_duration, data = lactation_metrics)

summary(lm_lac_dur)

lactation_metrics <- lactation_metrics %>%
  mutate(
    fitted_values = fitted(lm_lac_dur),
    residuals = resid(lm_lac_dur)
  )

# Plot lactation duration against residuals
ggplot(lactation_metrics, aes(x = lactation_duration, y = residuals)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Residuals vs Lactation Duration",
    x = "Lactation Durations (days)",
    y = "Residual (Observed - Predicted Total Milk)") +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line("black")
  )

# Plot fitted against residuals
ggplot(lactation_metrics, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Total Milk Production",
    y = "Residual"
  ) +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line("black")
  )

# --- Dry-off interval ---
# Academic debate centers on whether shorter (30-45 days) vs longer (>60 days) optimize the next lactation yield and cow health.

# Join each lactation's dry-off interval with the subsequent lactation
df <- lactation_metrics %>%
  arrange(AniLifeNumber, RemLactation_LacNumber) %>%
  group_by(AniLifeNumber) %>%
  mutate(next_yield = lead(total_milk_production)) %>%
  ungroup() %>%
  filter(!is.na(dry_off_interval), !is.na(next_yield))

# Check correlation between dry-off interval and next lactation yield
cor.test(df$dry_off_interval, df$next_yield, method = "spearman")


df <- lactation_metrics %>%
  arrange(AniLifeNumber, RemLactation_LacNumber) %>%
  group_by(AniLifeNumber) %>%
  mutate(
    next_yield = lead(total_milk_production)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(dry_off_interval),
    !is.na(next_yield)
  ) %>%
  # now add your dry-period bins to the filtered df
  mutate(
    dry_bin = case_when(
      between(dry_off_interval,   0,  44) ~ "< 45d",
      between(dry_off_interval,  45,  59) ~ "45-59d",
      between(dry_off_interval,  60,  74) ~ "60-74d",
      dry_off_interval       >= 75       ~ ">= 75d",
      TRUE                              ~ NA_character_
    ),
    dry_bin = factor(dry_bin,
                levels = c("< 45d", "45-59d", "60-74d", ">= 75d"),
                ordered = TRUE)
  )

ggplot(df, aes(x = dry_bin, y = next_yield)) +
  geom_boxplot() +
  labs(x = "Dry Period Length", y = "Next Lactation Total Yield") +
  theme_classic()

# Kurskall-Wallis Test to compare group means
kruskal.test(next_yield ~ dry_bin, data = df)

# Post-hoc Dunn Test
df %>%
  dunn_test(next_yield ~ dry_bin, p.adjust.method = "bonferroni")

# Fit a model
fit2 <- lm(next_yield ~ bs(dry_off_interval, knots = c(45, 60, 75)) + age_at_calving + avg_daily_yield, data = df)

summary(fit2)

# --- Measure statistical power in dry-off interval analysis ---
# Fit the ANOVA & pull out sums of squares
res.aov <- aov(next_yield ~ dry_bin, data = df)
ss <- summary(res.aov)[[1]]$`Sum Sq`
dfg <- summary(res.aov)[[1]]$Df

# Between-groups SS & within-group SS
ss_between <- ss[1]
ss_within <- ss[2]

# Eta-squared & Cohen's f
eta2 <- ss_between / (ss_between + ss_within)
f <- sqrt(eta2 / (1 - eta2))

cat("Observed eta^2 =", round(eta2, 3), "-> Cohen's f =", round(f, 3), "\n")

group_sizes <- df %>% count(dry_bin) %>% pull(n)
k <- length(group_sizes)
n_per_group <- mean(group_sizes)

cat("Groups (k) =", k, " Avg n per group =", round(n_per_group, 1), "\n")

# Compute achieved power
power_result <- pwr.anova.test(
  k = k,
  n = n_per_group,
  f = f,
  sig.level = 0.05
)

print(power_result)

# Find needed sample size for 80% power
pwr.anova.test(
  k = k,
  f = f,
  sig.level = 0.05,
  power = 0.80
)

# Adjusting dry-off interval bins to increase statistical power
df2 <- lactation_metrics %>%
  arrange(AniLifeNumber, RemLactation_LacNumber) %>%
  group_by(AniLifeNumber) %>%
  mutate(
    next_yield = lead(total_milk_production)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(dry_off_interval),
    !is.na(next_yield)
  ) %>%
  # now add your dry-period bins to the filtered df
  mutate(
    dry_bin = case_when(
      between(dry_off_interval,   0,  44) ~ "< 45d",
      between(dry_off_interval,  45,  75) ~ "45-75d",
      dry_off_interval       >= 75       ~ "> 75d",
      TRUE                              ~ NA_character_
    ),
    dry_bin = factor(dry_bin,
                     levels = c("< 45d", "45-75d", "> 75d"),
                     ordered = TRUE)
  )

ggplot(df2, aes(x = dry_bin, y = next_yield)) +
  geom_boxplot() +
  labs(x = "Dry Period Length", y = "Next Lactation Total Yield") +
  theme_classic()

# Kurskall-Wallis Test to compare group means
kruskal.test(next_yield ~ dry_bin, data = df2)

# Post-hoc Dunn Test
df2 %>%
  dunn_test(next_yield ~ dry_bin, p.adjust.method = "bonferroni")

# Fit a model
fit3 <- lm(next_yield ~ bs(dry_off_interval, knots = c(45, 75)) + age_at_calving + avg_daily_yield, data = df2)

summary(fit3)

# --- Measure statistical power in dry-off interval analysis ---
# Fit the ANOVA & pull out sums of squares
res.aov <- aov(next_yield ~ dry_bin, data = df2)
ss <- summary(res.aov)[[1]]$`Sum Sq`
dfg <- summary(res.aov)[[1]]$Df

# Between-groups SS & within-group SS
ss_between <- ss[1]
ss_within <- ss[2]

# Eta-squared & Cohen's f
eta2 <- ss_between / (ss_between + ss_within)
f <- sqrt(eta2 / (1 - eta2))

cat("Observed eta^2 =", round(eta2, 3), "-> Cohen's f =", round(f, 3), "\n")

group_sizes <- df2 %>% count(dry_bin) %>% pull(n)
k <- length(group_sizes)
n_per_group <- mean(group_sizes)

cat("Groups (k) =", k, " Avg n per group =", round(n_per_group, 1), "\n")

# Compute achieved power
power_result <- pwr.anova.test(
  k = k,
  n = n_per_group,
  f = f,
  sig.level = 0.05
)

print(power_result)

# Find needed sample size for 80% power
pwr.anova.test(
  k = k,
  f = f,
  sig.level = 0.05,
  power = 0.80
)

