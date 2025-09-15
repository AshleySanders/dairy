# Diagnostics Scripts

This folder contains diagnostic and validation scripts used to check, clean, and cross-verify dairy herd datasets (Lely SQL, Mil’Klic Supabase, farm accounting data).  
Each script focuses on a specific aspect of data integrity or reconciliation to ensure that downstream analyses are based on consistent and reliable inputs.

---

## Scripts Overview

### 00_check_for_dupes.R
- **Purpose:** Detect duplicate cow IDs (`AniLifeNumber`) across key derived tables.  
- **Checks:** Reports duplicated keys by table and provides counts.  
- **Use:** Ensures primary keys are unique before merging datasets.

---

### check_farm_births_by_calving_match.R
- **Purpose:** Verify consistency between farm birth records and calving data.  
- **Checks:** Flags mismatches between reported births and actual calving cycles.  
- **Use:** Helps confirm herd entry assumptions.

---

### check_missing.R
- **Purpose:** Summarize missing values across all columns of a given dataframe.  
- **Checks:** Counts `NA` and empty string values, computes percent missing.  
- **Use:** Identifies variables needing imputation or special handling.

---

### get_r_data_types.R
- **Purpose:** Inspect database table structures and map SQL fields to R data types.  
- **Checks:** Returns a summary table of variables and inferred R data classes.  
- **Use:** Supports construction of a data dictionary and schema consistency checks.

---

### lely_milk_prod_invoices.R
- **Purpose:** Compare Lely-recorded milk production with monthly farm invoices.  
- **Checks:** Aggregates invoice revenues, joins with milk deliveries, reconciles totals.  
- **Use:** Foundation for per-cow profitability analysis.

---

### mdp_visit_monthly_milk_prod_by_cow.R
- **Purpose:** Summarize Lely `PrmMilkDayProduction` data to monthly cow-level production.  
- **Checks:** Links daily visits to monthly totals.  
- **Use:** Inputs for longitudinal milk yield analyses.

---

### missing_lactation_data.R
- **Purpose:** Identify missing lactation records, dry-off dates, or calving intervals.  
- **Checks:** Counts cows with incomplete lactation data, joins with exit info.  
- **Use:** Ensures lactation-cycle completeness before survival or profitability analyses.

---

### mk_lely_lactation_data_comparison.R
- **Purpose:** Compare lactation cycle information between Mil’Klic and Lely-derived datasets.  
- **Checks:** Validates cycle counts, start dates, total production; flags mismatches.  
- **Use:** Confirms consistency across external and internal sources (Farm1, up to 2024-09-19).

---

### multiple_entries_exits.R
- **Purpose:** Detect cows with more than one recorded entry or exit event.  
- **Checks:** Lists duplicates in entry/exit tables.  
- **Use:** Ensures herd history is chronologically consistent.

---

## Usage Notes
- All scripts are standalone and can be run individually for diagnostics.  
- Dependencies: `dplyr`, `lubridate`, `DBI`, `glue`, `clipr`, `stringr` (check each script for specifics).  
- Inputs: Derived Lely SQL tables, Mil’Klic Supabase exports, farm financial records.  
- Outputs: Summaries, mismatches, and optional CSVs for further inspection.  

---

## Maintenance
- Keep metadata headers in each script consistent (`Script`, `Purpose`, `Notes`).  
- Update this README when adding or significantly revising diagnostic scripts.  
