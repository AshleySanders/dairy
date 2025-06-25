# Data Preparation Scripts: Herd Management Strategy

This directory contains data processing scripts for preparing structured feature tables used in herd-level strategy and outcome analysis. All scripts follow the [ProjectTemplate](http://projecttemplate.net/) conventions and are organized sequentially.

---

## 📁 Scripts Overview

### `01_save_sql_tables.R`

- **Purpose**: Extract and prepare core raw data tables from the Lely SQL database for analysis.
- **Data Sources**:
  - `PrmMilkDayProduction` – Daily milk production
  - `HemAnimal` – Animal identity, birthdate, activity flag
  - `RemLactation` – Lactation cycle metadata
- **Output**:
  - `milk_cows` — A joined table combining milk production and animal identity data  
  *(Note: This object is stored in memory, not saved to `cache/`)*
  
---

### `02_cow_features_construction.R`

- **Purpose**: Construct a `cow_features` table containing cow-level feature variables from Lely and Supabase data.
- **Input**: `milk_cows` from previous script (must be run first)
- **Feature Categories**:
  - Milk performance: `total_milk`, `avg_daily_milk`, `avg_monthly_milk`, `milk_span_days`, etc.
  - Milk quality: `fat_pct_avg`, `protein_pct_avg`
  - Animal profile: `AniLifeNumber`, `AniGenId`, `AniBirthday`, `birth_year`, `AniActive`
  - Lifecycle metadata: `entry_date`, `exit_date`, `exit_code`, `slaughter_date`, `weight`
  - History flags: `has_history_data` (from Supabase)
- **Join Sources**:
  - `animals_history` (filtered for Aubépine `customer_id` and dairy race `66`)
  - `animals_slaughter`
- **Cleaning Steps**:
  - Whitespace and format standardization for `AniLifeNumber` joins
  - Logic fixes: If cow has exit date or slaughter date, `AniActive` is set to `FALSE`
  - Missing `exit_code` is filled as `"B"` if slaughtered
- **Output**:
  - `data/cow_features.rds`

---

## 📌 Notes

- Scripts are modular — run `01_` before `02_`
- Final output in `cow_features.rds` will be used in downstream profiling and modeling
- Large intermediate objects (e.g., `milk_cows`) may benefit from being cached to improve reproducibility and performance

---

## 🛠 Next Steps

- Add new script `03_cow_outcomes_construction.R` to create target variables for strategy analysis
- Integrate Mil’Klic reproduction data when scraping is complete
- Summarize herd-level features from `cow_features` for cross-farm comparison


