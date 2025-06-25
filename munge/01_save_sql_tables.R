# Title: Save SQL Tables for Dairy Project
# Description: This script loads milk production data from a Lely database, along with animal metadata and lactation records.

# ProjectTemplate auto-executes this on load, so avoid reloading data unnecessarily


# Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

# Load milk production data from Lely
milk_all <- dbGetQuery(lely, "
  SELECT
    MdpId,
    MdpAniId,
    MdpProductionDate,
    MdpDayProduction,
    MdpDayProductionMAvg,
    MdpMilkings,
    MdpFatPercentage,
    MdpProteinPercentage
  FROM PrmMilkDayProduction
  ORDER BY MdpAniId, MdpProductionDate
")


# Load animal metadata
HemAnimal <- dbGetQuery(lely, "
  SELECT
    AniId,
    AniLifeNumber,
    AniActive,
    AniGenId,
    AniBirthday
  FROM HemAnimal
  ORDER BY AniId")

# Load lactation records
lactation <- dbGetQuery(lely, "
  SELECT
    LacId,
    LacAniId,
    LacNumber,
    LacDryOffDate,
    LacCalvingDate,
    LacColostrumDate,
    LacRemarks
  FROM RemLactation
")

# Join milk data to cow identity info
milk_cows <- milk_all %>%
  left_join(
    HemAnimal %>% select(AniId, AniLifeNumber, AniActive, AniGenId, AniBirthday),
    by = c("MdpAniId" = "AniId")
  )