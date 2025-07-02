library(dplyr)
library(lubridate)
library(tidyr)

lactation_data <- dbGetQuery(lely, "
  SELECT LacId, LacAniId, LacNumber, LacDryOffDate, LacCalvingDate, LacColostrumDate, LacRemarks
  FROM RemLactation
  ORDER BY LacAniId, LacNumber
")

lactation_animal <- dbGetQuery(lely, "
  SELECT
    RemLactation.LacId,
    RemLactation.LacAniId,
    RemLactation.LacNumber,
    RemLactation.LacDryOffDate,
    RemLactation.LacCalvingDate,
    RemLactation.LacColostrumDate,
    RemLactation.LacRemarks,
    HemAnimal.AniLifeNumber,
    HemAnimal.AniBirthday,
    HemAnimal.AniKeep,
    HemAnimal.AniGenId,
    HemAnimal.AniActive,
    HemAnimal.AniMotherLifeNumber
  FROM RemLactation
  INNER JOIN HemAnimal
    ON HemAnimal.AniId = RemLactation.LacAniId
  ORDER BY HemAnimal.AniId
")



# Convert date columns to Date type
lactation_animal <- lactation_animal %>%
mutate(
  LacDryOffDate = as.Date(LacDryOffDate, format = "%Y-%m-%d"),
  LacCalvingDate = as.Date(LacCalvingDate, format = "%Y-%m-%d"),
  LacColostrumDate = as.Date(LacColostrumDate, format = "%Y-%m-%d"),
  AniBirthday = as.Date(AniBirthday, format = "%Y-%m-%d"))

# Calculate lactation length in days
lactation_animal <- lactation_animal %>%
  mutate(
    lactation_length_days = case_when(
      LacNumber > 0 ~ as.numeric(LacDryOffDate - LacCalvingDate),
      TRUE ~ NA_real_
    )
  )


# Calculate age of cow at first calving
first_calving <- lactation_animal %>%
  filter(LacNumber == 1, !is.na(AniBirthday), !is.na(LacCalvingDate)) %>%
  group_by(AniLifeNumber) %>%
  slice_min(LacCalvingDate, with_ties = FALSE) %>%
  summarise(
    age_at_first_calving = interval(AniBirthday, LacCalvingDate) %/% months(1),
    .groups = "drop"
  )

lactation_animal <- lactation_animal %>%
  left_join(first_calving, by = "AniLifeNumber")

View(lactation_animal)

summary(lactation$LacDryOffDate)


# Get pregnancy data
pregnancy <- dbGetQuery(lely, "
    SELECT PreLacId, PreDate, PreInsId, PreRemark
    FROM RemPregnancy
")

# Get insemination data
insem_lactation <- dbGetQuery(lely, "
  SELECT
    RemInsemination.*,
    RemLactation.LacAniId,
    RemLactation.LacCalvingDate,
    RemLactation.LacNumber,
    RemLactation.LacDryOffDate,
    RemLactation.LacColostrumDate,
    RemLactation.LacRemarks,
    HemAnimal.AniLifeNumber,
    HemAnimal.AniBirthday,
    HemAnimal.AniKeep,
    HemAnimal.AniGenId,
    HemAnimal.AniActive,
    HemAnimal.AniMotherLifeNumber
  FROM RemInsemination
  INNER JOIN RemLactation
    ON RemLactation.LacId = RemInsemination.InsLacId
  INNER JOIN HemAnimal
    ON HemAnimal.AniId = RemLactation.LacAniId
  ORDER BY HemAnimal.AniId
")


# Convert date columns to Date type
insem_lactation <- insem_lactation %>%
  mutate(
    LacDryOffDate = as.Date(LacDryOffDate, format = "%Y-%m-%d"),
    LacCalvingDate = as.Date(LacCalvingDate, format = "%Y-%m-%d"),
    LacColostrumDate = as.Date(LacColostrumDate, format = "%Y-%m-%d"),
    AniBirthday = as.Date(AniBirthday, format = "%Y-%m-%d"),
    InsDate = as.Date(InsDate, format = "%Y-%m-%d")
  )


# Calculate lactation length in days
insem_lactation <- insem_lactation %>%
  mutate(
    lactation_length_days = case_when(
      LacNumber > 0 ~ as.numeric(LacDryOffDate - LacCalvingDate),
      TRUE ~ NA_real_
    )
  )
# Calculate age of cow at first calving
first_calving <- insem_lactation %>%
  filter(LacNumber == 1, !is.na(AniBirthday), !is.na(LacCalvingDate)) %>%
  group_by(AniLifeNumber) %>%
  summarise(age_at_first_calving = interval(min(AniBirthday), min(LacCalvingDate)) %/% months(1), .groups = "drop")

insem_lactation <- insem_lactation %>%
  left_join(first_calving, by = "AniLifeNumber")

# Calculate the interval between calving and next insemination
    # Get first insemination date per lactation
insem_first <- insem_lactation %>%
  group_by(LacAniId, LacNumber) %>%
  summarise(first_insem = min(InsDate), .groups='drop')

  # Calculate the interval
insem_first <- insem_first %>%
  left_join(
    lactation_data %>%
      select(LacAniId, LacNumber, LacCalvingDate),
    by = c("LacAniId", "LacNumber")
  ) %>%
  mutate(
    calving_to_insem = interval(LacCalvingDate, first_insem) %/% days(1),
    calving_to_insem = ifelse(calving_to_insem < 20 | calving_to_insem > 200, NA, calving_to_insem)
  )

   # Join the first insemination data back to the main dataset
insem_lactation <- insem_lactation %>%
  left_join(
    insem_first %>% select(LacAniId, LacNumber, calving_to_insem),
    by = c("LacAniId", "LacNumber")
  ) %>%
  group_by(LacAniId, LacNumber) %>%
  fill(calving_to_insem, .direction = "downup") %>%
  ungroup()

# Calculate the number of dry days before the next lactation
insem_lactation <- insem_lactation %>%
  group_by(LacAniId) %>%
  arrange(LacNumber) %>%
  mutate(
    next_lactation_dry_days = as.numeric(LacCalvingDate - lag(LacDryOffDate)),
    next_lactation_dry_days = ifelse(next_lactation_dry_days < 0 | next_lactation_dry_days > 120, NA, next_lactation_dry_days)
  ) %>%
  ungroup()


# Calculate the total number of lactations for each animal
insem_lactation <- insem_lactation %>%
  group_by(AniLifeNumber) %>%
  mutate(
    number_lactations = max(LacNumber, na.rm = TRUE)
  ) %>%
  ungroup()

# Join lactation and insemination data to the pregnancy data
insem_lac_preg <- insem_lactation %>%
  left_join(pregnancy, by = c("InsId" = "PreInsId"))

# Create a dataframe that provides a summary per cow with the latest lactation date
unique_cow_summary <- insem_lac_preg %>%
  group_by(AniLifeNumber) %>%
  summarise(
    AniBirthday = first(AniBirthday),
    number_lactations = max(number_lactations, na.rm = TRUE),
    age_at_first_calving = first(age_at_first_calving),
    latest_lactation_date = max(LacCalvingDate, na.rm = TRUE),
    avg_inseminations = mean(!is.na(InsDate), na.rm = TRUE),
    avg_lactation_length_days = mean(lactation_length_days, na.rm = TRUE),
    avg_calving_to_insem_days = mean(calving_to_insem, na.rm = TRUE),
    avg_next_lactation_dry_days = mean(next_lactation_dry_days, na.rm = TRUE),
    .groups = "drop"
  )