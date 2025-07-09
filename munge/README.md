# 🧪 Data Preparation Scripts: Herd Management Strategy

This directory contains modular R scripts used to prepare cleaned and structured datasets for cow-level and farm-level herd management strategy analysis. All scripts follow [ProjectTemplate](http://projecttemplate.net/) conventions and are executed in order.

---

## 📁 Script Overview

### `01_save_sql_tables.R`
- **Purpose**: Extracts and saves core SQL tables from the Lely milking system and prepares the `milk_cows` base table by joining daily milk production with core animal metadata.
- **Data Sources**:
  - `PrmMilkDayProduction`, `HemAnimal`, `RemLactation`
- **Output**:
  - `milk_cows` (stored in memory)

---

### `02_reproduction.R`
- **Purpose**: Constructs cow-level reproduction timelines from Lely data and calculates foundational reproductive metrics.
- **Key Calculations**:
  - `CalculatedLactationCycle` (adjusted)
  - `age_at_first_calving`
  - `lactation_length_days`
  - `dry_off_interval` (for post- and pre-2018 cycles)
- **Sources**:
  - `RemLactation`, `RemInsemination`, `RemPregnancy`, `HemAnimal`
- **Output**:
  - `lactation_animal`, `lactation_summary_post2018`, `lactation_summary_all`

---

### `03_mdp_visit_monthly_milk_prod_by_cow.R`
- **Purpose**: Resolves milk yield discrepancies by aligning daily milk production (`MDP`) with actual milk conserved based on `DeviceVisit`. Prior to November 2020, a correction factor of 0.959 is applied to estimate conserved milk.
- **Output**:
  - `full_milk_by_cow`

---

### `04_cow_features_construction.R`
- **Purpose**: Builds cow-level features used to assess management strategy. Aggregates milk production, reproductive, identity, and history data into a single dataset.
- **Features Included**:
  - **Milk production**: `avg_daily_milk`, `avg_milkings_per_day`, `milk_span_days`
  - **Milk quality**: `fat_pct_avg`, `protein_pct_avg`
  - **Reproduction**: `number_lactations`, `avg_dry_interval`, `age_first_calving_months`, `cohort`
  - **Lifecycle**: `entry_date`, `exit_date`, `exit_code`, `age_at_exit_days`, `AniActive`, `has_history_data`
  - **Cleaning**: Uses `clean_ani()` to standardize cow IDs across all datasets
- **Joins**:
  - `animals_history`, `animals_slaughter` (Supabase)
  - `lactation_summary_post2018`, `lactation_summary_all`
- **Output**:
  - `data/cow_features.rds`

---

### `05_cow_outcomes_construction.R`
- **Purpose**: Builds outcome variables linked to profitability, health, and retention. Final outcome dataset will be used for modeling strategy effectiveness.
- **Examples**:
  - `prod_decline_90d`, `high_yield_flag`, `short_span_flag`, `lifetime_milk_yield`, `profit_margin_est`
- **Output**:
  - `data/cow_outcomes.rds` *(planned)*

---

## ✅ Execution Notes

- Run scripts sequentially: `01_` → `02_` → `03_`...
- `clean_ani()` is used throughout to ensure ID consistency between Lely data and Supabase animal tables
- `valid_cows` ensures that only cows with valid and complete post-2018 lactation data are used in modeling
- Longitudinal summaries include both post-2018 and all-historical data where relevant
- All dates are parsed as `Date` type using `lubridate`

---

## 📌 Next Steps

### 🔧 Features (High Priority)
- [ ] Integrate **insemination features**:
  - Number of inseminations per cycle
  - First insemination timing
  - Calving-to-insemination interval
- [ ] Compute **calving intervals** using RemLactation and cross-referenced insemination data

### 🎯 Outcomes (In Progress)
- [ ] Finalize lifetime productivity and profitability indicators
- [ ] Validate outcome alignment with feature logic (e.g., consistent `exit_code`, drop-off patterns)

### 📦 Additions
- [ ] Add `06_farm_profiles.R` script to aggregate features by farm
- [ ] Integrate invoice data once permission is granted and parsing structure is finalized

---

*Last updated: 2025-07-09*
