# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based epidemiological study analyzing the effects of GLP-1 receptor agonists (semaglutide) on depression outcomes in patients with MDD and TRD. It uses clinical data exported from a research EHR system (CCM tables) stored locally at `/media/studies/ehr_study/uploaded-data/`.

## Running Scripts

Run the full pipeline from the project root:

```bash
Rscript install_dependencies.R  # first-time setup
Rscript main.R                  # runs full pipeline
```

Individual scripts can be run standalone but depend on upstream `OutputData/*.rds` files being present.

## Configuration (`config.json`)

All parameterization is in `config.json` (local overrides in `config_local.json`):

- `target_drug` — The treatment drug ("Semaglutide")
- `comparator_drugs` — Array of 7 comparator drug classes (DPP4i, GLP1RA, Insulins, Metformin, SGLT2i, SU, TZD)
- `nontreatment_group` — Label for the no-treatment arm ("Nontreatment")
- `data_pull_date` — EHR snapshot date (YYYY-MM-DD); used in file naming
- `files` — Maps 16 named inputs to file paths (6 `Data/` reference files + 10 external CCM CSVs)

## Shared Utilities (`helper_functions.R`)

Every analysis script sources `helper_functions.R`. Key functions:

- `check_schema(schema, table_name, file_path)` — Validates CSV columns against `column_schema.csv`; stops on mismatch
- `make_col_types(schema, table_name)` — Builds a `readr::cols()` spec from the schema data frame
- `apply_recode(data, mapping, table_name)` — Normalizes `SimpleGenericName` via `medication_recode.csv`
- `check_recode(data, mapping, table_name)` — Warns and writes unmapped names to `OutputData/missing_recodes-{table}.csv`
- `interpret_nb(model, term, outcome, comparison)` — Prints rate ratio + CI + p-value for a NB GLM term
- `interpret_pwp(model, term, outcome, comparison)` — Same for PWP Cox model hazard ratios
- `check_od(model)` / `check_poisson(model)` / `check_mc(model)` — Model diagnostic helpers
- `my_table1(this.data, my_strata, filename, varsToFactor)` — Creates Table 1 CSV and HTML (writes to `OutputData/` and `html_tables/`)
- `summarize_interaction(model, mod_label)` / `summarize_strata(emm_contrast, mod_var)` — Sensitivity analysis interaction helpers

`workbench_functions.txt` is retained for All of Us Research Workbench runs (provides `notebook_setup()` and BigQuery helpers) but is not sourced by scripts in this local codebase.

## Data Reference Files (`Data/`)

| File | Purpose |
|---|---|
| `ps_covariates.csv` | 38 PS model covariates (6 continuous, 32 binary) |
| `column_schema.csv` | Expected column names and types for all CCM input tables |
| `medication_recode.csv` | Canonical drug name mappings (`raw_name` → `canonical_name`) |
| `Drug_ATC_Categories.csv` | ATC codes used to filter drug classes |
| `DrugClasses.csv` | Drug class hierarchy |
| `period_info.csv` | 5 outcome time windows (`bgn_win`, `end_win` in days from index) |
| `psych_proc_tiers-manualEdit-MPP.csv` | CPT code acuity tiers for psychiatric procedures |
| `var_name_to_pretty.csv` | Variable name → display label mappings for reports |

## Pipeline Architecture (`main.R` orchestrated)

Outputs go to `OutputData/*.rds` (intermediate data) and `Reports/*.html` (rendered Rmd reports).

1. **`prepData.R`** — Schema-validates and ingests CCM CSVs → `antidepressant_table.rds`, `antipsychotics_table.rds`, `hydrochlorothiazide_table.rds`, `treatments_table.rds`, `dte_cohort_data.rds`, `mdd_data.rds`, `diag_table.rds`

2. **`get_TRD.R`** — Identifies TRD patients from antidepressant consecutive periods

3. **`get_Antidepressant_Treatment_Timeline.R`** / **`get_Hydrochlorothiazide_Treatment_Timeline.R`** — Build consecutive medication instance and period records

4. **`get_Nontreatment_Timelines.R`** — Adds nontreatment arm via index date emulation (samples time-from-eligibility distribution from target drug patients) → `dte_cohort_wNontreat_data.rds` (primary cohort used by most downstream scripts)

5. **`get_Diagnosis_Timeline.R`** — Appends diagnosis timeline flag variables to the cohort

6. **`analysis_Propensity_Scoring.R`** (loop over each comparator) → `PS_Matched_Dataset-{drug}.rds`, `PS_Weighted_Dataset-{drug}.rds`, `PS_Covariates-{drug}.csv`

7. **Outcome scripts** (run in parallel via `doParallel`):
   - `get_Outcomes_Visits.R` → `outcomes_visits.rds`
   - `get_Outcomes_PsychProc.R` → `outcomes_psych_proc.rds`
   - `get_Outcomes_MedChanges.R` → `outcomes_med_changes.rds`
   - `get_Outcomes_HCMedChanges.R` → `outcomes_hc_med_changes.rds`

8. **Analysis** (loop over comparators × outcomes × periods):
   - `analysis_Negative_Binomial_Regression.R` — Count outcomes (NB GLM, IRR)
   - `analysis_PWP_Gap_Time_Cox_Model.R` — Recurrent events (PWP gap-time Cox, HR)
   - `analysis_Sensitivity_Med_Changes.R` / `analysis_Sensitivity_Med_Changes-Change_Type.R` — Sensitivity analyses

Reports are rendered via `rmarkdown::render()` with `params = list(...)` per comparator. `explore_*.R` and `plot-*.R` scripts are for EDA and do not produce files used by downstream analyses.

## Key Conventions

**Script header pattern:**
```r
library(tidyverse)
library(jsonlite)
source("helper_functions.R")
config <- fromJSON("config.json")
```

**Function pattern:** `get_*()` and `analysis_*()` functions save results to disk via `save()` or `write.csv()` and `return(invisible())`. Most accept an `overwrite` flag (default `TRUE`) for conditional recomputation.

**RDS payload:** When loading with `load("file.rds")`, the variable name restored matches the name used at save time — check the creating script for the exact name.

**Consecutive instances:** A gap > 180 days between prescriptions of the same `SimpleGenericName` starts a new `consecutive_instance`. Used in TRD identification and treatment timeline scripts.

**Study cohort label:** Always `"{target_drug} vs {comparator_group}"` (e.g., `"Semaglutide vs DPP4i"`).

**Outcome periods** (from `period_info.csv`):
- `a`: −180 to 0 days (baseline)
- `b`: 15–90 days post-index
- `c`: 15–180 days post-index
- `d`: 15–360 days post-index (primary analysis period)
- `e`: 180–360 days post-index

**Treatment variable:** `treatment = 1` (target drug, Semaglutide), `treatment = 0` (comparator/nontreatment).

**Patient IDs:** `PatientDurableKey` in CCM medication/diagnosis tables; `person_id` in some cohort assembly and sensitivity scripts. These refer to the same patients; join carefully when combining tables.

**PS matching:** Caliper = 0.1 × SD(logit PS); acceptable covariate balance threshold = SMD < 0.1.
