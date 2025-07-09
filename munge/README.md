# 🧪 Data Preparation Scripts: Herd Management Strategy

This directory contains modular R scripts used to prepare cleaned and structured datasets for cow-level and farm-level herd management strategy analysis. All scripts follow [ProjectTemplate](http://projecttemplate.net/) conventions and are executed in order.

---

## 📁 Script Overview

### `01_save_sql_tables.R`
- **Purpose**: Extracts core tables from Lely's SQL database and joins milk production with animal metadata.
- **Data Sources**:
  - `PrmMilkDayProduction`, `HemAnimal`, `RemLactation`
- **Output**:
  - `milk_cows` (stored in memory, not saved)

---

### `02_reproduction.R`
- **Purpose**: Prepares reproductive metadata including lactation cycles, insemination records, calving intervals, and early summaries.
- **Calculations**:
  - `lactation_length_days`
  - `age_at_first_calving`
  - `calving_to_insem`, `next_lactation_dry_days`
  - `number_lactations`
- **Sources**:
  - `RemLactation`, `RemInsemination`, `RemPregnancy`, `HemAnimal`
- **Output**:
  - `lactation_animal`, `insem_lactation`, and `unique_cow_summary` in memory

---

### `03_mdp_visit_monthly_milk_prod_by_cow.R`
- **Purpose**: Used to determine that the monthly milk production by cow data from PrmMilkDayProduction includes discarded milk after comparing it with a joined dataset (MilkVisit * DeviceVisit) that calculates conserved milk. Then, this script does a vertical join between MDP estimates based on a calculated ratio of 0.959 of kept to total milk produced for dates prior to November 2020, when the MilkVisit * DeviceVisit data begins.
- **Output**:
  - `full_milk_by_cow`
  
---

### `04_cow_features_construction.R`
- **Purpose**: Builds cow-level feature variables from milk, identity, and history data.
- **Features Included**:
  - Milk performance: `total_milk`, `avg_daily_milk`, `avg_monthly_milk`
  - Milk quality: `fat_pct_avg`, `protein_pct_avg`
  - Animal status: `AniActive`, `birth_year`, `has_history_data`
  - Lifecycle: `entry_date`, `exit_date`, `slaughter_date`, `exit_code`, `weight`
- **Joins**:
  - `animals_history`, `animals_slaughter` (from Supabase)
- **Output**:
  - `data/cow_features.rds`

---

### `05_cow_outcomes_construction.R`
- **Purpose**: Calculates outcome variables per cow to evaluate herd strategy profitability.
- **Examples**:
  - `prod_decline_90d`, `high_yield_flag`, `short_span_flag`, `cohort`
- **Output**:
  - `data/cow_outcomes.rds` *(planned)*

---


## ✅ Execution Notes

- Run scripts sequentially: `01_` → `02_` → `03_`...
- `milk_cows`, `lactation_animal`, and other intermediate objects are stored in memory unless explicitly saved
- For memory-intensive joins, SQL-based joins or filtered extracts may be preferred
- Reproduction data from Mil’Klic may be integrated in a future step

---

## 📌 To Do

- Finalize `cow_outcomes.rds` with lactation-linked variables
- Add `farm_profiles.R` script to aggregate features by farm
- Integrate invoice data once available

