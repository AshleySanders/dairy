# `lib/` Directory — Dairy Herd Management Project

This folder contains **core library scripts** for the Cockpit Agriculture – Dairy Herd Management Strategy project.  
It provides shared utilities, database ingestion, caching logic, and farm-specific data corrections.  

All scripts are modular and designed for reproducibility across multiple farm datasets.

---

## File Overview

| File                           | Purpose                                                                                                                                                                                                 |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `01-libraries.R`               | Loads required R packages for SQL querying, data wrangling, visualization, and caching.                                                                                                                  |
| `02_ingest_global_data.R`      | Queries and caches global Supabase tables (`animals`, `animals_history`, `animals_slaughter`, `gl_entries`, `milk_invoice`). These tables are reused across all farms.                                   |
| `03_save_farm_tables.R`        | Main ingestion script for querying and caching **farm-level Lely and Supabase tables**. Saves standardized objects with prefixes (`fm1_`, `fm5_`, etc.) for reproducible downstream use.                  |
| `04_farm1_fix_hemanimal_ids.R` | Farm-specific correction script for **Farm 1**. Fixes known mismatches in `HemAnimal` identifiers to ensure valid joins. Must be run after caching raw tables.                                           |
| `05_farm5_fix_data_errors.R`   | Farm-specific correction script for **Farm 5**. Fixes missing `AniLifeNumber` country codes, adds missing birthdates and mothers, corrects `birth_year`, and removes a non-existent cow from the dataset. |
| `globals.R`                    | Defines optional global constants or settings used across scripts.                                                                                                                                       |
| `helpers.R`                    | Provides helper functions (e.g., `clean_ani()`, `assign_and_cache()`) for standardized cleaning, naming, and caching of datasets.                                                                        |
| `README.md`                    | This file — describes the purpose and contents of the `/lib/` directory.                                                                                                                                 |

---

##  Usage Guidelines

* Always **source the farm config file** (`farmX_config.R`) before running any script in this directory.  
* Do **not** place these scripts inside `munge/` — they are not intended to be re-executed automatically by `load.project()`.  
* Use `assign_and_cache()` for all farm-specific tables to ensure consistent naming conventions.  
* Keep all credentials and `DBI::dbConnect()` calls inside `/config/farm_config_farmX.R`, **not in this directory**.  

---

## Reusability

* Scripts are **parameterized and modular**, allowing them to be reused across farms with consistent naming (`fm1_`, `fm5_`, etc.).  
* Adding a new farm requires only:  
  1. A new config file in `/config/`  
  2. Running `02_ingest_global_data.R` + `03_save_farm_tables.R`  
  3. Adding optional farm-specific fix scripts if needed (`04_farmX_fix_data_errors.R`).  

---
