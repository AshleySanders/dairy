# dairy

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

## 📁 Project Structure (ProjectTemplate format)

dairy/
├── data/ # Final output datasets (e.g., cow_features.rds)
├── cache/ # Intermediate .rds objects for performance
├── munge/ # Scripts to load, clean, and engineer data
│ ├── 01_save_sql_tables.R
│ └── 02_cow_features_construction.R
├── reports/ # Future reporting and visualization outputs
├── src/ # Optional: helper functions
├── README.md # Project overview (this file)
└── .Rproj # RStudio project file


---

## 🛠 Technologies Used

- **R** (with ProjectTemplate)
- `dplyr`, `lubridate`, `stringr`, `here`
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
- [x] `cow_features` table built (milk + status + lifecycle)
- [x] Slaughter records integrated and used to patch missing exit data
- [ ] `cow_outcomes` table (outcome metrics like yield persistence) — coming next
- [ ] Mil’Klic reproduction data — pending scraping

---

## Next Steps

- Create `cow_outputs` table for yield, lifecycle, and persistency segmentation
- Incorporate reproduction history from Mil'Klic (once scraped)
- Prepare herd-level summaries for strategy benchmarking
- Model herd profitability and decision outcomes

---

## Maintainer

**Ashley Sanders**  
[GitHub](https://github.com/AshleySanders)  [LinkedIn](https://www.linkedin.com/in/ashleyrsanders/)

---

## 📜 License

MIT License is open source and free to use with attribution. 
