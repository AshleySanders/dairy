# 🧪 Data Preparation Scripts: Herd Management Strategy

This directory contains modular R scripts used to prepare cleaned and structured datasets for cow-level and farm-level herd management strategy analysis. All scripts follow [ProjectTemplate](http://projecttemplate.net/) conventions and are executed in order.

---

## Script Overview

### `01_animals_meta_data_creation.R`
- **Purpose**: Create clean animal-level metadata for a specific farm from Supabase tables, including herd history, demographics, and exit info.
- **Steps**:
  - Extract and clean `animals`, `animals_history`, and `animals_slaughter`
  - Derive entry/exit metadata and age-at-exit
  - Filter for dairy cows (exclude males)
  - Join slaughter data to supplement missing exit info
  - Validate joins with `HemAnimal` (from Lely)
- **Outputs**:
  - `cache/dairy_meta_farm1.RData`

---

### `02_reproduction.R`
- **Purpose**: Construct a clean, cow-level dataset of reproductive events (insemination, pregnancy, calving).
- **Key Steps**:
  - Attach inseminations to lactation cycles
  - Join pregnancy confirmations
  - Deduplicate and flag successful inseminations/pregnancies
  - Add `birth_date` from Supabase metadata
- **Outputs**:
  - `data/insem_lac_preg.rds`
  - `data/insem_lac_preg.csv`

---

### `03_lactation_metrics.R`
- **Purpose**: Generate per-lactation-cycle metrics combining milk production and reproduction data.
- **Metrics**:
  - Reproduction: `age_at_calving`, `n_insem`, `n_failed_insem`, `n_failed_pregnancies`, `calving_interval_days`, `calving_to_insem_days`, `dry_off_interval`
  - Milk production: `total_milk_production`, `avg_daily_yield`, early/mid lactation yields, fat/protein %, delivered milk with correction factor
  - Lifecycle flags: `last_lactation`, `still_milking`
- **Outputs**:
  - `lactation_metrics` (cached)

---

### `04_dairy_health_measures.R`
- **Purpose**: Aggregate raw health event data into per-cow summary variables.
- **Details**:
  - Filters out preventive events (e.g., dry-offs, vaccinations)
  - Produces per-cow health problem counts and flags
- **Outputs**:
  - `dairy_health_problems` (filtered events)
  - `cow_health_summary` (per-cow aggregates)

---

### `05_cow_features_build_and_combine.R`
- **Purpose**: Build per-cow feature tables per farm and combine into a multi-farm dataset.
- **Details**:
  - Farm-specific features constructed from `lactation_metrics`, `insem_lac_preg`, `cow_health_summary`, `dairy_meta`
  - Appends features into a combined `cow_features` dataset with farm provenance
- **Outputs**:
  - `<farm>_cow_features` (cached and RDS/CSV files)
  - `cow_features` (multi-farm combined table)

---

### `06_cow_outcomes_construction.R`
- **Purpose**: ⚠️ Prototype script for building outcome variables linked to profitability, health, and retention.
- **Status**: **Incomplete — missing cost data. Not production-ready.**
- **Current Outputs**:
  - Preliminary outcome variables (`high_yield_flag`, `short_span_flag`, lifetime milk summaries)
- **Planned Enhancements**:
  - Add cost features (feed, vet, labor)
  - Profitability indicators (`profit_margin_est`, marginal returns)
- **Outputs (provisional)**:
  - `data/cow_outcomes.rds`

---

## Execution Notes

- Run scripts sequentially: `01_` → `02_` → `03_` …
- `clean_ani()` is used throughout to standardize cow IDs across Lely and Supabase datasets.
- `dairy_meta` scripts are farm-specific; manual corrections may be required via `/lib` scripts before caching.
- Incomplete scripts (e.g., `06_cow_outcomes_construction.R`) should **not be run** until flagged as complete.

---

## Next Steps

### 🎯 Outcomes (In Progress)
- [ ] Finalize lifetime productivity and profitability indicators
- [ ] Incorporate cost data to complete `cow_outcomes` construction
- [ ] Validate alignment of outcomes with feature logic (e.g., `exit_code`, milk drop-off patterns)

### 📦 Additions
- [ ] Add farm-level profiling (`farm_profiles.R`)
- [ ] Integrate invoice data once access and parsing structure are finalized

---

*Last updated: 2025-09-15*