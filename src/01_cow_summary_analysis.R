# --- Defining a "normal" cow life cycle on the farm. ---
library(rstatix)
library(ggpubr)

# 1. purchased or raised on the farm ?
table(cow_features$entry_code)

View(cow_features %>%
       filter(is.na(entry_code)))

# 2. Distribution + mean of age at first calving
summary(cow_features$age_at_first_calving)
hist(cow_features$age_at_first_calving,
     main = "Cow's Age at First Calving",
     xlab = "Age (months)",
     ylab = "Number of Cows")
boxplot(cow_features$age_at_first_calving)

cow_features %>% identify_outliers(age_at_first_calving) %>% select(c(AniBirthday, age_at_first_calving)) %>% print(n=75)

# Remove outliers before calculating the summary stats for age at first calving

valid_calving <- cow_features %>%
  filter(cohort != "pre-2016",
         entry_code != "A",
         !is.na(AniLifeNumber),
         AniLifeNumber != "")

summary(valid_calving$age_at_first_calving)

hist(valid_calving$age_at_first_calving,
     main = "Age at First Calving",
     xlab = "Age (months)",
     ylab = "Number of Cows")

# Examine distribution & outliers of cow's age at first insemination (according to Lely)
first_insem_outliers <- cow_features %>% identify_outliers(age_at_first_insem) %>%
  select(AniLifeNumber, AniBirthday, cohort, age_at_first_insem, age_at_first_calving, entry_code)

View(first_insem_outliers %>% filter(cohort != "pre-2016"))

summary(valid_calving$age_at_first_insem)

hist(valid_calving$age_at_first_insem, main = "Age at First AI (Months)")

boxplot(valid_calving$age_at_first_insem, horizontal = TRUE, xlab = "Age at First AI (Months)")

# Examine relationships between first insemination (successful or not) and first calving ages
ggplot(valid_calving, aes(x = InsDate, y = age_at_first_insem)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  labs(
    title = "Age at First AI Over Time",
    x = "Insemination Date",
    y = "Age (Months)"
  ) +
  theme_classic()

ggplot(valid_calving, aes(x = age_at_first_insem, y = age_at_first_calving)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm") +
  labs(
    x = "Age at First AI (Months)",
    y = "Age at First Calving (Months)"
  ) +
  theme_classic()

# Examine relationships between first successful insemination & first calving ages
valid_insem <- valid_calving %>%
  filter(age_first_successful_insem < 25)

ggplot(valid_insem, aes(x = age_first_successful_insem, y = age_at_first_calving)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "lm") +
  labs(
    x = "Age at First Successful AI (Months)",
    y = "Age at First Calving (Months)"
  ) +
  theme_classic()


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

valid_dry_off <- lactation_metrics %>%
  filter(
    !is.na(dry_off_interval),
    AniLifeNumber != ""
  )

ggplot(valid_dry_off, aes(x = lactation_duration, y = dry_off_interval)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "gam") +
  labs(
    title = "Dry-Off Interval as a Function of Lactation Duration",
    x = "Lactation Duration (Days)",
    y = "Dry-Off Interval (Days)"
  ) +
  theme_classic()


# Missing Data Analysis
na_dry_off <- lactation_metrics %>%
  filter(is.na(dry_off_interval),
         still_milking == FALSE,
         last_lactation == FALSE,
         AniLifeNumber != "")

# Outlier Identification
cow_features %>% identify_outliers(avg_dry_interval) %>%
  select(c(AniBirthday, avg_dry_interval)) %>%
  print(n=28)

# Outlier Analysis
dry_off_outliers <- lactation_metrics %>% identify_outliers(dry_off_interval)
nrow(dry_off_outliers)

View(dry_off_outliers)

# Examine relationship between number of inseminations and failed pregnancies for dry_off interval outliers
failed_conception <- table(dry_off_outliers$n_insem, dry_off_outliers$n_failed_pregnancies)
failed_conception

fisher.test(failed_conception, simulate.p.value = TRUE)

# Examine relationship between number of failed inseminations and failed pregnancies for dry-off interval outliers.
insem_preg_fail <- table(dry_off_outliers$n_failed_insem, dry_off_outliers$n_failed_pregnancies)
fisher.test(insem_preg_fail, simulate.p.value = TRUE)

# --- Examine interval between successful insemination and dry-off date ---

# interval between successful insemination & dry-off date
# For each cycle (InsLacId), get the first successful AI date
ins_per_cycle <- insem_lac_preg %>%
  filter(successful_insem, successful_pregnancy) %>%
  group_by(AniLifeNumber, InsLacId, CalculatedLactationCycle) %>%
  summarise(
    first_success = min(as.Date(InsDate)),
    .groups = "drop"
  )

# Shift it forward one cycle so it matches the next lactation’s index
cycle_ins_match <- ins_per_cycle %>%
  mutate(
    CalculatedLactationCycle = CalculatedLactationCycle + 1L
  )

# Join to your lactation_metrics and compute the intervals
lactation_with_intervals <- lactation_metrics %>%
  mutate(
    LacCalvingDate = as.Date(LacCalvingDate),
    dry_off_date   = as.Date(dry_off_date)
  ) %>%
  left_join(
    cycle_ins_match,
    by = c("AniLifeNumber", "CalculatedLactationCycle")
  ) %>%
  mutate(
    # Days from the AI (in cycle N) to the calving that starts cycle N+1
    insem_to_calving = as.integer(LacCalvingDate - first_success),
    # Days from that same AI to the dry-off of cycle N+1
    insem_to_dryoff  = as.integer(dry_off_date   - first_success)
  )

lactation_with_intervals %>%
  select(AniLifeNumber, CalculatedLactationCycle,
         first_success, LacCalvingDate, dry_off_date,
         insem_to_calving, insem_to_dryoff)

summary(lactation_with_intervals$insem_to_calving)

boxplot(lactation_with_intervals$insem_to_calving, horizontal = TRUE,
        main = "Interval Between Successful AI to Calving (Days)")

summary(lactation_with_intervals$insem_to_dryoff)

boxplot(lactation_with_intervals$insem_to_dryoff, horizontal = TRUE, main = "Interval Between Successful AI to Dry-Off")

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

# Examine a possible change in Farm 1's strategy in mid-2024

cow_features_2024 <- cow_features %>%
  filter(exit_date > as.Date("2024-01-01"))

ggplot(data = cow_features_2024, aes(x = exit_date, y = endmilk_to_exit_days)) +
  geom_point(alpha = 0.6, size = 3, color = "steelblue") +
  geom_smooth(method = "gam") +
  labs(title = "Number of Days Between the Last Milking and Exit in 2024",
       x = "Exit Date",
       y = "Number of Days") +
  theme_classic()

# Doublecheck and verify exit dates with slaughter dates.
exit_date_check <- cow_features %>%
  mutate(
    exit_check = interval(as.Date(slaughter_date), as.Date(exit_date)) %/% days(1)
  )

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

# Milk production over time
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

# 9. Milk production summary
summary(cow_features$avg_total_milk)

boxplot(cow_features$avg_total_milk, horizontal = TRUE,
        xlab = "Average Milk Production per Cow per Lactation (L)")

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

# 13. The script for the milk yield analysis is available in 02_single_variable_milk_yield_analysis.R

# 14. Health Problem Analysis
405 - sum(is.na(cow_features$n_health_problems))

summary(cow_features$n_health_problems)

boxplot(cow_features$n_health_problems, horizontal = TRUE,
        main = "Number of Health Problems")

cow_features <- cow_features %>%
  mutate(
    diagnosis_to_exit = interval(as.Date(date_last_diagnosis), as.Date(exit_date)) %/% days(1)
  )

summary(cow_features$diagnosis_to_exit)

# Simplify last diagnosis

cow_features <- cow_features %>%
  mutate(
    simplified_last_diagnosis = case_when(

      # — Mastitis of any form —
      str_detect(last_diagnosis, regex("mastitis", ignore_case=TRUE)) ~ "mastitis",

      # — Other udder issues (non-mastitis) —
      str_detect(last_diagnosis, regex("udder|teat|other disorders of the udder", ignore_case=TRUE)) ~ "udder disorder",

      # — Lameness / hoof & joint —
      str_detect(last_diagnosis, regex("lameness|pododermatitis|phlegmon|double sole|arthritis|parage", ignore_case=TRUE)) ~ "lameness",

      # — Reproductive & calving problems —
      str_detect(last_diagnosis, regex("fertility|ovarial cysts|retentio secundinarum|diseases related to calving
                                         |sectio caesarea|endometritis|dystocia|abortion|interoestrus|metritis",
                                       ignore_case=TRUE)) ~ "reproductive",

      # — Metabolic diseases —
      str_detect(last_diagnosis, regex("ketosis|acetonemia|metabolic|hyperketonemia", ignore_case=TRUE)) ~ "metabolic",

      # — Parasitic —
      str_detect(last_diagnosis, regex("trematode|parasite", ignore_case=TRUE)) ~ "parasitic",

      # — Respiratory / systemic infections —
      str_detect(last_diagnosis, regex("pneumonia|bronchopneumonia|infection|fever|abs[cç]es", ignore_case=TRUE)) ~ "infection",

      # — Digestive (abomasal displacement, for example) —
      str_detect(last_diagnosis, regex("abomasal displacement", ignore_case=TRUE)) ~ "digestive",

      # — None / no abnormality —
      str_detect(last_diagnosis, regex("no abnormality", ignore_case=TRUE)) ~ "none",

      # — Explicit “none recorded” —
      is.na(last_diagnosis) ~ NA_character_,

      # — Everything else —
      TRUE ~ "other"
    )
  )

last_diagnosis <- table(cow_features$simplified_last_diagnosis)

cow_features %>%
  na.omit(cols = "simplified_diagnosis") %>%   # drop NA diagnoses
  # count cows by simplified diagnosis
  count(simplified_last_diagnosis, name = "n") %>%
  # Arrange descending and set factor levels
  arrange(n) %>%
  mutate(
    simplified_last_diagnosis = fct_inorder(simplified_last_diagnosis)
  ) %>%
  # Plot
  ggplot(aes(x = simplified_last_diagnosis, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of Cows",
    title = "Last Diagnosis"
  ) +
  theme_classic()





