# `src/` — Analysis Scripts

This folder contains the **analysis layer** for the Cockpit Agriculture – Herd Management Strategy project.  
Scripts assume that data preparation and caching have been completed (see `munge/` and `lib/`).  
Each script focuses on a specific analytical question and is designed to be sourced interactively.

---

## Recommended Run Order

1) `01_cow_summary_analysis.R`  
2) `02_single_variable_milk_yield_analysis.R`  
3) `03_health_analysis.R`  
4) `04_repro_analysis.R`  
5) `05_culling_decision_analysis.R`  
6) `06_sale_strategy_analysis.R` / `07_sale_strategy_analysis.R` (choose based on data availability)

> **Prereqs:** Load/cached objects from data prep (e.g., `*_cow_features`, `*_lactation_metrics`, `insem_lac_preg`, `dairy_health_problems`, etc.).

---

## Script Summaries

### 01_cow_summary_analysis.R
**Purpose:** Describe and quantify a “normal” cow life cycle and highlight deviations affecting reproduction, production, and exit.

**What it does:**  
- Entry source (purchased vs raised), flags missing entries.  
- Repro milestones: age at first AI & calving; link first/first-successful AI to calving age.  
- Insemination load (avg per cow/cycle) vs number of lactations.  
- Lactation counts distribution; dry-off intervals (distributions/outliers & relation to duration; link successful AI → next cycle).  
- Exit pathway: days from last milking to exit; long-delay cases.  
- Average lactation duration trends; milk production patterns by time & cohort.  
- Milk production summaries; lifespan at exit; calving→AI interval; calving interval patterns (multi-farm bind).  
- Health problems: counts, recovery times, diagnosis simplification & plots.

**Inputs:** `fm5_cow_features`, `fm5_lactation_metrics`, `lactation_metrics`, `fm5_insem_lac_preg` (optional: `fm1_lactation_metrics`).  
**Outputs:** Summary tables, multiple ggplots, outlier data frames.  
**Deps:** `rstatix`, `ggpubr`, `ggplot2`, `dplyr`, `tidyr`, `forcats`, `lubridate`.  

---

### 02_single_variable_milk_yield_analysis.R
**Purpose:** Explore how individual factors relate to total milk per lactation and quantify effect sizes/significance.

**What it does:**  
- **Age at calving:** bin 2–10 yrs; ANOVA (or Kruskal–Wallis + Dunn); assumption checks; boxplots.  
- **Lactation duration:** LM + diagnostics; scatter + smoother.  
- **Dry-off interval:** next-cycle yield construction; Spearman correlation; Kruskal–Wallis + Dunn; spline LM; power analysis; contingency links to #AIs and failed pregnancies (BH-adjusted tests); boxplots.  
- **Calving→first AI:** Spearman + LM (covariate age_at_calving) + viz.  
- **Calving interval:** LOESS + mixed-effects model `milk ~ calving_interval + (1|cow)`.

**Inputs:** `fm5_lactation_metrics`, `combined_lactation_metrics`.  
**Outputs:** Model summaries, power estimates, posthoc tables, plots.  
**Deps:** `car`, `FSA`, `rstatix`, `splines`, `pwr`, `RVAideMemoire`, `lme4`, `ggplot2`, `dplyr`, `tidyr`, `broom`.  

---

### 03_health_analysis.R
**Purpose:** Analyze health problems in **Farm 1** (Farm 5 excluded due to unreliable stats); test how diagnosis probability varies by age.

**What it does:**  
- Join diagnoses with cow features; compute **age at diagnosis** and age bins (<2, 2–7+ yrs).  
- χ² tests on age-bin distributions; **mixed-effects logistic** (weighted & unweighted).  
- **Panel (cow-year) logistic regression** with spline age terms; predicted probabilities for ages 2–9 with CIs; plots.

**Inputs:** `fm1_cow_features`, `fm1_dairy_health_problems` (+ derived panel).  
**Outputs:** Model summaries, hist/box plots, predicted probability tables/plots.  
**Deps:** `lme4`, `rstatix`, `ggpubr`, `ggeffects`, `splines`, `ggplot2`.  
**Notes:** Outcome = `diag_event`; random intercept for `AniLifeNumber`.

---

### 04_repro_analysis.R
**Purpose:** Summarize insemination outcomes and calving spacing at cow level.

**What it does:**  
- Per-cow AI totals, failures, success_rate; histogram & summary.  
- First-AI conception efficiency per lactation → per-cow summary + plot.  
- Calving intervals (days/months) from consecutive calving dates; per-cow averages.

**Inputs:** `insem_lac_preg`.  
**Outputs:** `failed_insem_per_cow`, `repro_efficiency`, `calving_intervals`, `calving_summary`; plots.  
**Deps:** `dplyr`, `ggplot2` (optional `here`).  
**Assumptions:** Failed AI = `is.na(PreLacId)`; first AI = earliest `InsDate`; months = 30.44 days.

----

### 05_culling_decision_analysis.R
**Purpose:** Model factors predicting culling using a **Random Survival Forest (RSF)**.

**What it does:**  
- Builds survival features: time-to-exit (censored if still in herd), aggregated health events, imputed `n_insem`.  
- Fits RSF (logrank splitrule), evaluates C-index (OOB), KM curves by predicted risk quartile.  
- Profiles high- vs low-risk cows and maps to actionable management triggers.

**Inputs:** `cow_features`, `lactation_metrics`, `dairy_health_problems`.  
**Outputs:** Variable importance plot, KM curves, group summaries/plots.  
**Deps:** `survival`, `ranger`, `forcats`, `ggplot2`, `dplyr`, `tidyr`, `car`.  
**Notes:** RSF cannot handle NA predictors—ensure complete features.

---

### 06_sale_strategy_analysis.R
**Purpose:** Evaluate effect of fattening strategy on sale prices (market-adjusted) and identify key drivers.

**What it does:**  
- GAM trends for market adjustment; t-tests/ANOVA; decision tree & random forest.  
- Visualizes sale price over time, weight trends; evaluates threshold strategy.

**Inputs:** `cow_features.csv`, `last_milking_weight.csv`, `cow_sales_no_classification.csv`.  
**Outputs:** Summary stats, GAM results, ML models, plots.  
**Deps:** `dplyr`, `tidyr`, `ggplot2`, `mgcv`, `rpart`, `caret`, `car`, `randomForest`.  
**Notes:** Assumes 50% dressing % for live weight estimation.

---

### 07_sale_strategy_analysis_farm5.R
**Purpose:** Variant of 06 with a reduced input set; same objective and methods.

**Inputs:** `cow_features.csv`, `cow_sales_no_classification.csv`.  
**Outputs/Deps/Notes:** As in 06 (minus last_milking_weight).

---

## Conventions & Tips

- **ID Cleaning:** Upstream scripts standardize IDs via `clean_ani()`; ensure consistent joins before running.  
- **Missing Data:** Some models require complete cases; impute or filter as indicated in each script.  
- **Randomness:** Use `set.seed(123)` before procedures with simulated p-values or resampling.  
- **Persistence:** Save derived tables/models if re-used:
  ```r
  saveRDS(obj, "data/<name>.rds")
