# 🐄 Dairy Herd Management Strategy Analysis

This project analyzes dairy herd productivity and lifecycle patterns using milking machine data, herd records, and slaughter/exit data. The goal is to generate actionable insights into herd management strategies that drive profitability and sustainability across multiple farms.

---

## Objectives

- **Clean and reconcile** complex multi-source farm data (Lely, Supabase, slaughterhouse)
- **Construct cow-level feature tables** (milk performance, lifecycle status, activity flags)
- **Segment cows** into output profiles (e.g., high-yield, short-cycle, dropout)
- **Support cross-farm comparisons** of herd management decisions
- **Enable predictive modeling** of profitability and productivity

---

<pre>## 📁 Project Structure (ProjectTemplate format)

```dairy/
├── 01-load-project.R      # Entry script: choose farm config & load project
├── dairy.Rproj            # RStudio project file
│
├── cache/                 # Derived objects cached for performance
│                          # (e.g., lactation_metrics, cow_features)
│
├── config/                # Farm-specific configs + global project settings
│   ├── farm1_config.R
│   ├── farm2_config.R
│   ├── farm5_config.R
│   ├── global.dcf
│   └── README.md
│
├── data/                  # Raw input + derived data (CSV/SQL extracts/exports)
│
├── diagnostics/           # QA, validation & repair scripts
│                          # (duplicates, missing data, milk comparisons, etc.)
│
├── graphs/                # Plots and visual outputs
│
├── lib/                   # Core library scripts
│   ├── 01-libraries.R
│   ├── 02_ingest_global_data.R
│   ├── 03_save_farm_tables.R
│   ├── 04_farm*_fix_*.R
│   ├── globals.R
│   ├── helpers.R
│   └── README.md
│
├── logs/                  # Run logs and debugging outputs
│
├── munge/                 # Data preparation scripts
│   ├── 01_animals_meta_data_creation.R
│   ├── 02_reproduction_lely.R
│   ├── 03_lactation_cycle_measures.R
│   ├── 04_dairy_health_measures.R
│   ├── 05_cow_features_construction.R
│   ├── 06_cow_outcomes_construction.R
│   └── README.md
│
├── sql/                   # SQL scripts (Lely queries)
│
├── src/                   # Analysis scripts (final models)
│   ├── 01_cow_summary_analysis.R
│   ├── 02_single_variable_milk_yield_analysis.R
│   ├── 03_health_analysis.R
│   ├── 04_repro_analysis.R
│   ├── 05_culling_decision_analysis.R
│   ├── 06_sale_strategy_analysis.R
│   ├── 07_sale_strategy_analysis_farm5.R
│   └── README.md
│
└── README.md              # Project overview (this file)
```</pre>


---

## 🛠 Technologies Used

- **R** (with ProjectTemplate)
- `dplyr`, `lubridate`, `stringr`, `here`, etc.
- SQL queries from Lely milking machine database
- SQL herd history tables
- Git/GitHub for version control

---

## Current Data Artifacts

| File                        | Description                                   |
|-----------------------------|-----------------------------------------------|
| `data/cow_features.rds`     | Cow-level features (milk, identity, status)   |
| `data/animals_history.csv`  | Entry/exit records from Supabase              |
| `data/missing_milk.csv`     | Cows with calving dates but no milk records   |

---

## ✅ Current Status

- [x] Core SQL tables loaded (milk production, animal identity, lactation)
- [x] `cow_features` table built (milk + status + lifecycle) for two farms
- [x] Slaughter records integrated and used to patch missing exit data
- [x] Analysis of farm features complete for Farm1 & Farm5 plus some comparisons
- [x] Prepare herd-level summaries for strategy benchmarking
- [x] Model decisions and outcomes
- [ ] `cow_outcomes` table (outcome metrics like yield persistence)

---

## Next Steps

- Create `cow_outputs` table for yield, lifecycle, and persistency segmentation
- Model herd profitability and decision outcomes

---

## Maintainer

**Ashley Sanders**  
[GitHub](https://github.com/AshleySanders)  [LinkedIn](https://www.linkedin.com/in/ashleyrsanders/)

---

## 📜 License

MIT License is open source and free to use with attribution. 
