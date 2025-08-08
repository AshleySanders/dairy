# 05_sale_strategy_analysis

cow_sales <- cow_features %>%
  select(AniLifeNumber, AniBirthday, age_at_exit, exit_date, exit_code, slaughter_date, weight, birth_year, cohort, time_to_exit_days) %>%
  mutate(price_per_kg = (selling_price / weight))