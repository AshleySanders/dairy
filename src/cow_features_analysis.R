# --- Defining a "normal" cow life cycle on the farm. ---
library(rstatix)
library(ggpubr)

# 1. purchased or raised on the farm ?
table(cow_features$entry_code)

# 2. Distribution + mean of age at first calving
summary(cow_features$age_at_first_calving)
hist(cow_features$age_at_first_calving,
     main = "Cow's Age at First Calving",
     xlab = "Age (months)",
     ylab = "Number of Cows")
boxplot(cow_features$age_at_first_calving)

cow_features %>% identify_outliers(age_at_first_calving) %>% select(c(AniBirthday, age_at_first_calving)) %>% print(n=75)

# Remove outliers before calculating the summary stats for age at first calving

cow_features %>%
  filter(AniBirthday > as.Date("2016-11-01")) %>%
  pull(age_at_first_calving) %>%
  summary() %>%
  hist()

# 3. Number of inseminations needed per lactation cycle
summary(cow_features$avg_insem)
boxplot(cow_features$avg_insem, horizontal = TRUE,
        main = "Average number of artificial inseminations per cycle per cow")
hist(cow_features$avg_insem,
     main = "Average number of artificial inseminations per cycle per cow",
     xlab = "Average # of AIs",
     ylab = "Number of Cows")

# 4. Number of milking cycles per cow
summary(cow_features$number_lactations)
hist(cow_features$number_lactations,
     main = "Number of Lactation Cycles per Cow",
     xlab = "Number of Lactations",
     ylab = "Number of Cows")


# 5. Dry-off interval
summary(cow_features$avg_dry_interval)
boxplot(cow_features$avg_dry_interval, horizontal = TRUE,
        main = "Average Dry-Off Intervals by Cow",
        xlab= "Number of Days")
cow_features %>% identify_outliers(avg_dry_interval) %>%
  select(c(AniBirthday, avg_dry_interval)) %>%
  print(n=28)


# 6. Fattening or immediate sale to abattoir?
summary(cow_features$endmilk_to_exit_days)
ggplot(data = cow_features, aes(x=exit_date, y=endmilk_to_exit_days)) +
  geom_point(aes(color = "red")) +
  labs(title = "Number of Days Between the Last Milking and Exit",
       x = "Exit Date",
       y = "Number of Days") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "dark grey")
  )
boxplot(cow_features$endmilk_to_exit_days, horizontal = TRUE,
        main = "Number of Days Between the Last Milking and Exit")

cow_features %>% identify_outliers(endmilk_to_exit_days) %>%
  select(c(AniLifeNumber, AniBirthday, exit_date, endmilk_to_exit_days))

# 7. Average number of days in each cow’s lactation cycle over the lifetime of the cow
summary(cow_features$avg_lactation_duration)
boxplot(cow_features$avg_lactation_duration, horizontal = TRUE,
        main = "Average Lactation Duration",
        xlab = "Number of Days")

lactation_metrics %>% filter(lactation_duration < 216) %>% select(c(AniLifeNumber, milk_production_start_date, milk_production_end_date, exit_date))

View(lactation_metrics %>% filter(identify_outliers(lactation_duration)))

lactation_outliers <- lactation_metrics %>% identify_outliers(lactation_duration)
View(lactation_outliers)

# Remove lactation cycles for cows who are still milking.
lactation_metrics %>%
  filter(still_milking == FALSE) %>%
  pull(lactation_duration) %>%
  summary()

# Examine potential trends over time.
ggplot(cow_features, aes(x=AniBirthday, y = avg_lactation_duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Cow's Average Lactation Duration by Birth Year",
       x = "Birth Year",
       y = "Number of days") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

ggplot(cow_features, aes(x=exit_date, y = avg_lactation_duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Cow's Average Lactation Duration Over Time",
       x = "Exit Date",
       y = "Number of Days") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )


# Examination of potential correlation between cow's age at calving and lactation duration

ggplot(lactation_metrics, aes(x = age_at_calving, y = lactation_duration,
                              color = last_lactation)) +
  geom_point() +
  scale_color_manual(
    name   = "Last Lactation",
    values = c(`FALSE` = "grey20", `TRUE` = "blue"),
    labels = c("Not Last", "Last")
  ) +
  labs(title = "Lactation duration versus age at calving",
       x =" Age at Calving",
       y = "Lactation Duration (days)") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line("black")
  )

ggplot(lactation_metrics, aes(x = last_lactation, y = lactation_duration)) +
  geom_boxplot() +
  labs(x = "Last Lactation?", y = "Duration (days)") +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

hist(lactation_metrics$age_at_calving)

# 8. Compare the younger cows' shorter lactation duration (above) with trends in milk production
summary(cow_features$avg_total_milk) # all cows

ggplot(cow_features, aes(x=AniBirthday, y = avg_total_milk)) +
  geom_point() +
  labs(title = "Milk Production",
       x = "Birthdate",
       y = "Milk Production (liters)") +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line("black")
  )

# 9. Milk production over time
ggplot(cow_features, aes(x=exit_date, y = avg_total_milk)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Milk Production Over Time",
       x = "Exit Date",
       y = "Milk Production (liters)") +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line("black")
  )

# 10. Lifespan of Cows
summary(cow_features$age_at_exit)
hist(cow_features$age_at_exit,
     main = "Cows' Age at Exit",
     xlab = "Age (Months)",
     ylab = "Number of Cows")

# 11. Calving to Insemination
summary(cow_features$avg_calving_to_insem)

# Visually examine the distribution - histogram
hist(cow_features$avg_calving_to_insem,
     main = "Average Time Between Calving & the Next AI",
     xlab = "Days",
     ylab = "Number of Cows")

# Boxplot of the distribution, which indicates outliers
boxplot(cow_features$avg_calving_to_insem, horizontal = TRUE,
        main = "Average Time Between Calving & the Next AI",
        xlab = "Number of Days")

# View outliers
cow_features %>% identify_outliers(avg_calving_to_insem) %>%
  select(c(AniLifeNumber, avg_calving_to_insem, avg_insem, insem_success_ratio)) %>% print(n = 18)

# Return to lactation cycle metrics to explore anomalies
lactation_metrics %>% identify_outliers(calving_to_insem_days) %>%
  select(c(AniLifeNumber, calving_to_insem_days, n_insem, n_failed_pregnancies)) %>% print(n=40)

# Test to examine assumption of correlation between the 3 variables

# Select only the 3 metrcis, drop any rows with NAs
tmp <- lactation_metrics %>%
  select(calving_to_insem_days, n_insem, n_failed_pregnancies) %>%
  na.omit()

# Pairwise Spearman correlations
cor_mat <- cor(tmp, method = "spearman")
print(cor_mat)

# Test each pair
cor.test(tmp$calving_to_insem_days, tmp$n_insem, method = "spearman",
         exact = FALSE, continuity = TRUE)
cor.test(tmp$calving_to_insem_days, tmp$n_failed_pregnancies, method = "spearman",
         exact = FALSE, continuity = TRUE)
cor.test(tmp$n_insem, tmp$n_failed_pregnancies, method = "spearman",
         exact = FALSE, continuity = TRUE)

# Visual check
pairs(tmp,
      panel = function(x,y,...) {
        points(x, y, pch=21, bg="grey80", col="grey40", ...)
        lines(stats::loess.smooth(x, y), col="red", lwd=1.5)
      },
      main = "Pairwise Relationships (Spearman)"
)

# Create a metric for services per conception to examine reproductive efficiency
# and summarize
lactation_metrics %>%
  mutate(services_per_conception = n_insem / (n_failed_pregnancies + 1)) %>%
  summarise(
    median = median(services_per_conception, na.rm = TRUE),
    IQR    = IQR(services_per_conception,      na.rm = TRUE)
  )

# Examine tail of the distribution to see cycles with multiple services:
lactation_metrics %>%
  mutate(svc_per_conc = n_insem / (n_failed_pregnancies + 1)) %>%
  count(svc_per_conc) %>%
  arrange(desc(svc_per_conc)) %>%
  head(10)


# 12. Calving interval patterns
summary(lactation_metrics$calving_interval_days)

# Visual analysis of the distribution & outliers
hist(lactation_metrics$calving_interval_days,
     main = "Interval Between Calvings",
     xlab = "Number of Days",
     ylab = "Number of Cows")

boxplot(lactation_metrics$calving_interval_days, horizontal = TRUE,
        main = "Interval Between Calvings",
        xlab = "Number of Days")



# Culling analysis - use survival model to determine which factors are most predictive of culling
