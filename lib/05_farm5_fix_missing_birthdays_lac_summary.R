# 12 rows in lactation metrics still didn't have cow birth dates (AniBirthday)
# Identify the missing cows and look for other identifying information that could be used to look them up in another table

# From the script in 03_lactation_cycle_measures, view the cows who still have missing birthdates
missing_birthdates <- (df_filled %>% filter(is.na(AniBirthday)) %>%  select(c(AniLifeNumber, AniBirthday)))

# See if these cows have an AniLifeNumber that appears in the animals table for this farm
missing_birthdates$AniLifeNumber %in% fm5_dairy_animals$national_number

# Dig deeper and look at the full data available for these cows
View(lm %>% filter(AniLifeNumber %in% missing_birthdates$AniLifeNumber))

lm_missing_cows <- lm %>% filter(AniLifeNumber %in% missing_birthdates$AniLifeNumber)

# These cows have AniIds but are missing AniLifeNumber in lactation_metrics. List the AniIds of the missing cows and save them
unique(lm_missing_cows$AniId)

missing_AniIds <- unique(lm_missing_cows$AniId)

# See if these cows have complete data in HemAnimal that didn't get transferred for some reason with the function in the script mentioned above.
View(HemAnimal %>% filter(AniId %in% missing_AniIds))

df_filled <- df_filled %>%
  mutate(
    AniLifeNumber = case_when(
      AniId == "133" ~ "FR4404288196",
      AniId == "274" ~ "FR4404288312",
      AniId == "356" ~ "FR4404288379",
      AniId == "459" ~ "FR4401541485",
      TRUE ~ AniLifeNumber
    ),
    AniBirthday = case_when(
      AniId == "133" ~ as.Date("2015-04-12"),
      AniId == "274" ~ as.Date("2017-10-09"),
      AniId == "356" ~ as.Date("019-01-31"),
      AniId == "459" ~ as.Date("2017-09-12"),
      TRUE ~ AniBirthday
    ),
    AniMotherLifeNumber = case_when(
      AniId == "274" ~ "FR4404288101",
      AniId == "356" ~ "FR4404288171",
      AniId == "459" ~ "FR4401541370",
      TRUE ~ AniMotherLifeNumber
    )
  )
