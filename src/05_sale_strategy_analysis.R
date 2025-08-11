# 05_sale_strategy_analysis

library(mgcv)
library(influence.ME)
library(car)

# Data:
# - cow_features
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


# Estimate live weight at time of slaughter
cow_sales <- cow_sales %>%
  mutate(
         est_dressing_percent = weight/MviWeight,
         est_exit_weight = weight/0.5, # Based on data from Farm 1
         est_weight_diff = est_exit_weight - MviWeight,
         )

lm_full_weight <- lm(est_exit_weight ~ MviWeight + fattened + age_at_exit + cohort, data=cow_sales)

# View summary stats of last milking weight and estimate live weight at exit:
summary(last_weight$MviWeight)
summary(cow_sales$est_exit_weight)

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

ggplot(data = cow_sales, aes(x = endmilk_to_exit_days, y = price_per_kg, fill = classification)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "gam") +
  labs(
    title = "Sale Price per Kilograph as a Function of Fattening Time",
    x = "Number of Days Between Last Milking and Exit",
    y = "Price (€/kg)") +
  theme_classic()

fattened_cows <- cow_sales %>%
  filter(endmilk_to_exit_days >= 30)

unfattened_cows <- cow_sales %>%
  filter(endmilk_to_exit_days < 30)

summary(fattened_cows$price_per_kg)
summary(unfattened_cows$price_per_kg)

t_test(price_per_kg ~ fattened, data = cow_sales, var.equal = TRUE, conf.level = 0.95)

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


