# 📁 `lib/` Directory — Dairy Herd Management Project

This folder contains core utility scripts, data caching logic, and database query handlers used throughout the **Cockpit Agriculture – Dairy Herd Management Strategy** project.

All scripts are modular and designed for reproducibility across multiple farm datasets.

---

## 📄 File Overview

| File                           | Purpose                                                                                                                                                                                                        |
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `01-libraries.R`               | Loads required R packages for SQL querying, data wrangling, and caching.                                                                                                                                       |
| `02_save_sql_tables.R`         | Main script for querying and caching raw data from Lely and Supabase databases using farm-specific config. All tables are dynamically named and cached using a consistent prefix (e.g., `fm1_`, `fm2_`, etc.). |
| `03_farm1_fix_hemanimal_ids.R` | Custom script for Farm 1 to correct inconsistencies in `HemAnimal` identifiers post-import. Referenced after initial caching.                                                                                  |
| `globals.R`                    | (Optional) Global constants or shared settings not tied to any specific farm.                                                                                                                                  |
| `helpers.R`                    | Contains helper functions like `clean_ani()` and `assign_and_cache()` to simplify processing and ensure consistent naming and caching behavior.                                                                |
| `README.md`                    | This file — describes the purpose and contents of the `/lib/` directory.                                                                                                                                       |

---

## 🛠️ Usage Guidelines

* Always **source the farm config file** (`farmX_config.R`) before running any script in this directory.
* Do **not** place these scripts inside `munge/` to avoid accidental re-execution during `load.project()`.
* Use `assign_and_cache()` for all farm-specific data to ensure proper prefixing and reproducibility.
* Keep all connection credentials and `DBI::dbConnect()` calls inside `config/farm_config_farmX.R`, **not in this directory.**

---

## 🔁 Reusability

* Scripts are parameterized to support analysis of multiple farms via consistent naming.
* Adding a new farm requires only a new config file in `/config/` and reusing the same scripts here.

