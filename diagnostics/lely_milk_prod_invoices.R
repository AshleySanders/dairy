# ------------------------------------------------------------------------------
# Script: lely_milk_prod_invoices.R
# Purpose: Compare Lely production data to invoice revenue to validate per-cow
#          profitability inputs and monthly aggregates.
# Notes:   Pulls lean tables from Lely DB, aggregates monthly sales, and joins
#          with delivered milk volumes for cross-checks.
# ------------------------------------------------------------------------------


library(dplyr)
library(lubridate)

# Create a lean dataframe from PrmDeviceVisit
device_visit <- dbGetQuery(lely, "
  SELECT
    DviId,
    DviAniId,
    DviDevId,
    DviFailure,
    DviVisitIdStamp
  FROM PrmDeviceVisit")

# Create a lean dataframe from PrmMilkVisit
milk_visit <- dbGetQuery(lely, "
  SELECT
    MviId,
    MviDviId,
    MviMilkYield,
    MviStartTime,
    MviMilkDestination,
    MviMilkDestReason
  FROM PrmMilkVisit")

# Create a lean datafrom from PrmMilkDayProduction just for this analysis
milk_day_production <- dbGetQuery(lely, "
  SELECT
    MdpAniId,
    MdpProductionDate,
    MdpDayProduction,
    MdpMilkings,
    MdpFatPercentage,
    MdpProteinPercentage
  FROM PrmMilkDayProduction")

# Note HemAnimal is already saved in memory for joins with the animal's national ID number.

# The following join cannot be completed in R. After completing the join in Visual Studio Code, the resulting table was 135GB.

# Join milk_visit -> device_visit -> milk_day_production -> HemAnimal
# milk_full <- milk_visit %>%
  # 1. Join with device_visit on MviDviId = DviId
  # left_join(device_visit, by = c("MviDviId" = "DviId"),
           # suffix = c("_mvi", "_dvi")) %>%

  # 2. Join with milk_day_production on DviAniId = MdpAniId
  # left_join(milk_day_production, by = c("DviAniId" = "MdpAniId"),
            # suffix = c("", "_mdp")) %>%

  # 3. Join with HemAnimal on MdpAniId = AniId
  # left_join(HemAnimal, by = c("MdpAniId" = "AniId"),
            # suffix = c("", "_ani"))

# Identify all invoices for milk sales
milk_sales_farm1 <- farm1_gl_credits %>%
  filter(gl_account_number == "70210852")

# Select relevant columns
milk_sales_farm1 <- milk_sales_farm1 %>%
  select(date, label, credit, gl_account_number)

View(milk_sales_farm1)


# Sum the revenues received at the end of each month
milk_sales_farm1 <- milk_sales_farm1 %>%
  mutate(
    year = year(date),
    month = month(date)
  )

last_dates <- milk_sales_farm1 %>%
  group_by(year, month) %>%
  summarise(
    last_day = max(date),
    .groups = "drop"
  )

milk_sales_farm1_monthly <- milk_sales_farm1 %>%
  inner_join(last_dates, by = c("year", "month")) %>%
  filter(date == last_day) %>%
  group_by(year, month, date) %>%
  summarise(
    total_revenue = sum(credit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, month)

# View the final result
View(milk_sales_farm1_monthly)

# Join monthly revenues received to total milk production data
milk_deliveries_farm1 <- read.csv(here("data", "milk_deliveries_2016_2024.csv"))

milk_deliveries_farm1 <- milk_deliveries_farm1 %>%
  mutate(date = as.Date(date))

milk_deliveries_farm1 <- milk_deliveries_farm1 %>%
  left_join(
    milk_sales_farm1_monthly %>%
      select(date, total_revenue),
    by = "date"
  ) %>%
  rename(revenue = total_revenue)

View(milk_deliveries_farm1)

# Updated milk quantities, classifications, and revenues in the milk_production_monthly_farm1 manually directly from Terrena invoices from the farm's portal on the company website.
milk_production_monthly_farm1 <- read.csv(here("data", "milk_production_monthly_farm1.csv"))

