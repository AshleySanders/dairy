# Diagnostics Directory

This folder contains **quality assurance (QA) and diagnostic scripts** used to validate data integrity, reconcile inconsistencies, and cross-check results across systems.  
These scripts are **not part of the core modeling pipeline** but provide important transparency and confidence in the data preparation process.

---

## Scripts Overview

### 1. `check_duplicates.R`
- **Purpose**: Utility to check for duplicate `AniLifeNumber` keys in key farm tables.
- **Notes**: Calls `check_dups()` on multiple datasets and reports counts of duplicates.

### 2. `01_check_farm_births_by_calving_match.R`
- **Purpose**: Validate whether calf birthdates match mother calving dates.
- **Inputs**: `lactation_animal` and `animals_meta_farm1` (from cache).
- **Outputs**: Table of mother–calf pairs with calving–birth proximity.
- **Notes**: Used for QA only; not for feature creation.

### 3. `check_missing.R`
- **Purpose**: Report missing values (NA or `""`) for each variable in a dataframe.
- **Outputs**: Counts and percentages of missing data, sorted descending.

### 4. `get_r_data_types.R`
- **Purpose**: Inspect a database table, infer R data types, and copy results.
- **Notes**: Pulls one row to detect types; optionally copies summary to clipboard.

### 5. `lely_milk_prod_invoices.R`
- **Purpose**: Compare Lely production data to invoice revenue to validate per-cow profitability inputs and monthly aggregates.
- **Notes**: Joins Lely DB tables with invoice sales and delivered milk volumes for cross-checks.

### 6. `lely_milk_comparison.R`
- **Purpose**: Quantify the difference between total and conserved milk reported by Lely across systems.
- **Inputs**: Multiple CSV datasets of monthly milk production and kept milk estimates.
- **Outputs**: 
  - `full_milk_by_cow.csv` (unified dataset of conserved milk by cow)  
  - `full_milk_monthly` (total monthly yield across cows)  
  - Summary plot (MDP vs Visit-based differences)
- **Notes**: Filters partial months; produces unified dataset for modeling and aggregation.

### 7. `missing_lactation_data.R`
- **Purpose**: Identify cows with missing milk production after calving.
- **Inputs**: Joins lactation, milk, and animal exit data.
- **Notes**: Diagnostics for dry-off dates, lactation length, and calving-to-insemination intervals.

### 8. `mk_lely_lactation_data_comparison.R`
- **Purpose**: Compare lactation cycle info between Mil’Klic and Lely-derived data.
- **Notes**: Validates cycle counts, start dates, and total milk production per cow; identifies mismatches.  
  Focused on Farm1 up to 2024-09-19.

### 9. `multiple_entries_exits.R`
- **Purpose**: Explore entry/exit patterns in `animals_history` to identify multiple entries/exits per cow (possible inter-farm transfers or re-entries).
- **Inputs**: `animals_history` (from Supabase).
- **Outputs**: `multiple_entries_exits.csv` plus summary tables of entry/exit code associations.
- **Notes**: Supports interpretation of missing data and integration across farms.

---

## Usage Notes
- These scripts are **standalone diagnostics**: run them directly to investigate issues.
- They may require data from `cache/` or raw SQL extracts in `data/`.  
- Outputs are typically **CSV summaries and plots** saved locally for inspection.  
- Results from diagnostics are **not consumed by the modeling pipeline** but guide cleaning decisions and provide an audit trail.

---

## Best Practices
- Run diagnostics after major ETL steps to confirm data integrity.  
- Use outputs to document assumptions or decisions in `DECISIONS.md`.  
- When adding new diagnostics, include a metadata block and update this README.
