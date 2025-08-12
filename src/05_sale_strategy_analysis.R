# 05_sale_strategy_analysis

library(mgcv)
library(influence.ME)
library(car)
library(randomForest)

# Data:
# -> cow_features
last_milking_weight <- read.csv(here("data", "last_milking_weight.csv")) # Generated in VSC

last_milking_weight <- last_milking_weight %>%
  mutate(
    AniLifeNumber = clean_ani(AniLifeNumber),
    LastMilkingDate = as.Date(LastMilkingDate)
  )

cow_sales <- cow_features %>%
  select(AniLifeNumber, AniBirthday, age_at_exit, exit_date, exit_code, slaughter_date, weight, birth_year, cohort, endmilk_to_exit_days, selling_price, classification, category) %>%
  mutate(price_per_kg = selling_price / weight,
         fattened = if_else(endmilk_to_exit_days >= 30, TRUE, FALSE, missing = NULL)) %>%
  filter(!is.na(price_per_kg))

valid_sales_ids <- (cow_sales$AniLifeNumber)

# Create a table of each cow's final weight at last milking that matches the cow IDs in cow_sales
last_weight <- last_milking_weight %>%
  filter(AniLifeNumber %in% valid_sales_ids)

cow_sales <- cow_sales %>%
  left_join(last_weight %>%
              select(AniLifeNumber, MviWeight, LastMilkingDate),
            by = "AniLifeNumber")

# read the data set in that contains the correct classifications that were previously missing
cow_sales_classifications <- read_csv(here("data", "cow_sales_no_classification.csv"))

cow_sales_updated <- cow_sales %>%
  left_join(cow_sales_classifications %>% select(AniLifeNumber, classification),
            by = "AniLifeNumber") %>%
  mutate(
    classification = coalesce(classification.x, classification.y)
  ) %>%
  select(-c(classification.x, classification.y))

cow_sales <- cow_sales_updated



# View summary stats of last milking weight and dressed weight:
summary(last_weight$MviWeight)
summary(cow_sales$weight)

# Initial, simple analysis of fattened versus unfattened revenue per kg
table(cow_sales$fattened)

boxplot(price_per_kg ~ fattened, data = cow_sales,
        main = "Price Summary for Fattened and Non-Fattened Cows",
        ylab = "Price (€/kg)")

ggplot(data = cow_sales, aes(x = exit_date, y = selling_price)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "gam") +
  labs(
    title = "Total Sale Price per Cow Over Time",
    x = "Exit Date",
    y = "Total Sale Price (€)"
  ) +
  theme_classic()

ggplot(data = cow_sales, aes(x = endmilk_to_exit_days, y = selling_price)) +
  geom_point(alpha = 0.4, color = "magenta") +
  geom_smooth(method = "gam") +
  labs(
    title = "Total Sale Price per Cow by Fattening Time",
    x = "Days Between Last Milking and Exit Date",
    y = "Total Sale Price (€)"
  ) +
  theme_classic()

ggplot(data = cow_sales, aes(x = exit_date, y = price_per_kg)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "gam") +
  labs(
    title = "Sale Price per Kilogram Over Time",
    x = "Exit Date",
    y = "Sale Price (€/kg)"
  ) +
  theme_classic()

ggplot(data = cow_sales, aes(x = endmilk_to_exit_days, y = price_per_kg, fill = cohort)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "gam") +
  labs(
    title = "Sale Price per Kilogram Over Time",
    x = "Days Between Last Milking and Exit",
    y = "Sale Price (€/kg)"
  ) +
  theme_classic()

ggplot(data = cow_sales, aes(x = endmilk_to_exit_days, y = selling_price, fill = cohort)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "gam") +
  labs(
    title = "Total Sale Price by Cohort Over Time",
    x = "Days Between Last Milking and Exit",
    y = "Total Sale Price (€)"
  ) +
  theme_classic()

fattened_cows <- cow_sales %>%
  filter(endmilk_to_exit_days >= 30)

unfattened_cows <- cow_sales %>%
  filter(endmilk_to_exit_days < 30)

summary(fattened_cows$price_per_kg)
summary(unfattened_cows$price_per_kg)

summary(fattened_cows$weight)
summary(unfattened_cows$weight)

summary(fattened_cows$selling_price)
summary(unfattened_cows$selling_price)

boxplot(weight ~ fattened, data = cow_sales,
        main = "Dressed Weight (kg) by Fattening Strategy")

boxplot(selling_price ~ fattened, data = cow_sales,
        main = "Total Sale Price (€) by Fattening Strategy")

ggplot(data = cow_sales, aes(x=endmilk_to_exit_days, y = weight, color = cohort)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "gam") +
  labs(
    title = "Weight by Number of Days between Last Milking & Exit"
  ) +
  theme_classic()

ggplot(data = cow_sales, aes(x = exit_date, y = weight, color = fattened)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "gam") +
  labs(
    title = "Dressed Weight over Time",
    x = "Exit Date",
    y = "Dressed Weight (kg)"
  ) +
  theme_classic()

ggplot(data = cow_sales, aes(x = exit_date, y = MviWeight, color = fattened)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "gam") +
  labs(
    title = "Last Milking Weight over Time",
    x = "Exit Date",
    y = "Weight (kg) at Last Milking"
  ) +
  theme_classic()

t_test(price_per_kg ~ fattened, data = cow_sales, var.equal = TRUE, conf.level = 0.95)

t_test(weight ~ fattened, data = cow_sales, var.equal = TRUE, conf.level = 0.95)

t_test(selling_price ~ fattened, data = cow_sales, var.equal = TRUE, conf.level = 0.95)

fisher.test(table(cow_sales$fattened, cow_sales$classification), simulate.p.value = TRUE)



# Prepare data for GAM modeling
cow_sales <- cow_sales %>%
  mutate(
    exit_date = as.Date(exit_date),
    slaughter_date = as.Date(slaughter_date),
    fattened = as.factor(fattened),
    cohort = as.factor(cohort),
    classification = as.factor(classification)
  )

# Fit a GAM with a smoothing term for time
gam_model <- gam(price_per_kg ~ fattened + s(as.numeric(exit_date)) + classification + age_at_exit + cohort, data = cow_sales)
summary(gam_model)

# Check assumptions
par(mfrow = c(1, 2))
plot(gam_model)
qqnorm(residuals(gam_model))
qqline(residuals(gam_model))

vif(lm(price_per_kg ~ fattened + exit_date + age_at_exit + cohort, data = cow_sales))

# Check potential interactions
gam_model_i <- gam(price_per_kg ~ fattened * cohort + classification + s(as.numeric(exit_date)) + age_at_exit, data = cow_sales)
summary(gam_model_i)

# Check assumptions
par(mfrow = c(1, 2))
plot(gam_model_i)
qqnorm(residuals(gam_model_i))
qqline(residuals(gam_model_i))

# Plot residuals vs fitted values
plot(gam_model$fitted.values, residuals(gam_model))
abline(h=0, col='red')

# Identify outliers or influential points with leverage measures
x_resid <- residuals(gam_model_i)
outliers <- which(abs(x_resid) > 2)
print(outliers)

gam.check(gam_model_i)


# Create a new GAM model to understand if/how last milking weight, fattening strategy, exit_date, and classification may effect total selling price

gam_model2 <- gam(
  selling_price ~
    s(as.numeric(exit_date)) +    # smooth trend over time
    MviWeight +                   # linear effect of weight
    fattened +                    # factor (TRUE/FALSE)
    classification,               # factor
  data = cow_sales,
  method = "REML"
)

summary(gam_model2)
plot(gam_model2)

# Check assumptions
vif(lm(selling_price ~ fattened + exit_date + MviWeight + classification, data = cow_sales))
gam.check(gam_model2)

plot(gam_model2$fitted.values, residuals(gam_model2))
abline(h=0, col='red')

# --- Examine all variables importance in predicting selling price ---
rf_model <- randomForest(selling_price ~ MviWeight + exit_date + age_at_exit + fattened + endmilk_to_exit_days + classification + cohort, data = cow_sales, importance = TRUE)

importance(rf_model)
varImpPlot(rf_model)

# Explore effects of age on selling price
cow_sales <- cow_sales %>%
  mutate(
    exit_age_quartile = case_when(
      age_at_exit < 53 ~ "< 53m",
      age_at_exit >= 53 & age_at_exit < 67 ~ "53-67m",
      age_at_exit >=67 & age_at_exit < 84 ~ "67-83m",
      age_at_exit > 84 ~ "84m +"
    ),
    exit_age_quartile = as.factor(exit_age_quartile)
  )

boxplot(selling_price ~ exit_age_quartile, data = cow_sales,
        main = "Sale Price by Age Group",
        xlab = "Age Group (months)",
        ylab = "Sale Price (€)")

# ANOVA test for selling price differences by age group
age_price_aov <- aov(selling_price ~ exit_age_quartile, data = cow_sales)
summary(age_price_aov)

# Assumptions Check
# Get residuals
resid <- residuals(age_price_aov)

# QQ plot
qqnorm(resid)
qqline(resid)

# Homogeneity of variance (homoscedasticity)
leveneTest(selling_price ~ exit_age_quartile, data = cow_sales)

# As long as assumptions hold, move on to a parametric posthoc test
tukey_hsd(age_price_aov)


# Estimate live weight at time of slaughter
cow_sales <- cow_sales %>%
  mutate(
    est_dressing_percent = weight/MviWeight,
    est_exit_weight = weight/0.5, # Based on data from Farm 1
    est_weight_diff = est_exit_weight - MviWeight,
  )

lm_full_weight <- lm(est_exit_weight ~ MviWeight + fattened + age_at_exit + cohort, data=cow_sales)

