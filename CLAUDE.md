# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R-based epidemiological study of the effect of GLP-1 receptor agonists (semaglutide) on depression outcomes in patients with MDD and TRD. Clinical inputs come from a research SQL Server database (Epic-derived Caboodle-style tables) read via ODBC; the pipeline falls back to local CSVs when the DB block is absent from the config.

## Running Scripts

```bash
Rscript -e 'renv::restore()'   # first-time dependency install (renv)
Rscript main.R                 # runs full pipeline
```

`.Rprofile` sources `renv/activate.R`, so R sessions in this directory automatically use the project library. Individual scripts can be run standalone but depend on upstream `OutputData/*.rds` files being present. There is no `install_dependencies.R`.

## Configuration

- `config-example.json` is the tracked template. Copy to `config.json` (git-ignored) and edit before running.
- Top-level keys:
  - `target_drug` — treatment drug (`"Semaglutide"`).
  - `comparator_drugs` — 7 comparator classes (DPP4i, GLP1RA, Insulins, Metformin, SGLT2i, SU, TZD).
  - `nontreatment_group` — label for the no-treatment arm (`"Nontreatment"`).
  - `data_pull_date` — EHR snapshot date (YYYY-MM-DD).
  - `eligibility_inclusion_diagnoses` — diagnosis keys driving `diag_table` wide-pivot and `mdd_data` eligibility flags.
  - `database` — ODBC connection (`driver`, `server`, `database`, `trusted_connection`, `timeout`). If this block is omitted, `prepData.R` reads each `files.*` entry as a CSV path instead.
  - `files` — 8 `Data/` reference CSVs + 8 source tables (`med_table`, `diag_table`, `mdd_data`, `treatment_overlap_table`, `dte_cohort_data`, `nonswitch_periods`, `psych_proc`, `encounter_table`). The source-table values are SQL object names when `database` is set (e.g. `dbo.MedicationTable`) or file paths otherwise.

## Shared Utilities (`helper_functions.R`)

Every analysis script sources `helper_functions.R`. Key functions:

- **Schema + I/O (DB/CSV-dispatch)**:
  - `check_schema_table(schema, table_name, config, conn)` — dispatches to `check_schema_sql` (uses `SELECT TOP 0`) or `check_schema` (reads CSV header). Stops on column mismatch.
  - `read_table(config, col_schema, table_name, conn)` — reads via `DBI::dbGetQuery` or `readr::read_csv`, returning a typed data frame.
  - `make_col_types(schema, table_name)` — builds a `readr::cols()` spec from the schema rows (used on CSV paths).
  - `apply_col_types(data, schema, table_name)` — coerces DB rows to R types from the schema.
- **Recoding**:
  - `apply_recode(data, mapping, table_name)` — normalizes `SimpleGenericName` via `medication_recode.csv` (columns: `table`, `raw_name`, `canonical_name`, `subclass`).
  - `check_recode(data, mapping, table_name)` — warns on unmapped names and writes them to `OutputData/missing_recodes-{table}.csv`.
- **Model interpretation**: `interpret_nb()` (rate ratio + CI + p from a NB GLM term) and `interpret_pwp()` (hazard ratio from a PWP Cox term). Both hardcode the phrase "Semaglutide treatment" in their printed interpretation — update if changing the target drug.
- **Diagnostics**: `check_od()`, `check_poisson()`, `check_mc()`.
- **Reporting**: `my_table1(this.data, my_strata, filename, varsToFactor, ...)` writes `OutputData/{filename}.csv`, `OutputData/{filename}-frmt.rds`, and `html_tables/{filename}.html`. Create `html_tables/` before first use.
- **Sensitivity**: `summarize_interaction(model, mod_label)` and `summarize_strata(emm_contrast, mod_var)`.
- **Misc**: `sig_code()` (significance stars), `my_cut()` (bucketed factor), `number_to_viridis()` (color helper).

`workbench_functions.txt` is retained from All of Us Research Workbench runs and is not sourced locally.

## Data Reference Files (`Data/`)

| File | Purpose |
|---|---|
| `ps_covariates.csv` | 36 PS model covariates (6 continuous, 30 binary) |
| `column_schema.csv` | Expected columns + types for each source table (`med_table`, `diag_table`, `mdd_data`, `treatment_overlap_table`, `dte_cohort_data`, `nonswitch_periods`, `psych_proc`, `encounter_table`) |
| `medication_recode.csv` | Canonical drug name map: `raw_name` → `canonical_name`, keyed by `table` |
| `Drug_ATC_Categories.csv` | ATC codes used to filter drug classes |
| `DrugClasses.csv` | Drug class hierarchy |
| `period_info.csv` | 5 outcome time windows (`period`, `bgn_win`, `end_win` in days from index) |
| `psych_proc_tiers-manualEdit-MPP.csv` | CPT code acuity tiers for psychiatric procedures |
| `var_name_to_pretty.csv` | Variable name → display label mappings for reports |

`Data_Diary/prepData-schema.xlsx` is the human-readable schema reference. `generate_pipeline_map.py` produces `pipeline_map.csv` / `file_order.csv` (script dependency DAG). `.claude/settings.json` locks `Read` permission to the project tree (`Read(./**)` allowed, `Read(/**)` denied) and disables unsandboxed commands — do not attempt to read system paths like `/media/...`.

## Pipeline Architecture (`main.R` orchestrated)

Outputs go to `OutputData/*.rds` (intermediate data) and `Reports/*.html` (rendered Rmd reports). `main.R` creates `OutputData/` if missing but not `Reports/` or `html_tables/`.

1. **`prepData.R`** — Opens an ODBC connection (if `config$database` is set), schema-validates every source table, then builds:
   - `antidepressant_table.rds`, `antipsychotics_table.rds`, `hydrochlorothiazide_table.rds` (ATC-filtered, recoded)
   - `treatment_{drug}_table.rds` for each `all_drugs` entry (from `treatments_table`, split by `PharmaceuticalSubclass`)
   - `diag_table.rds`, `mdd_data.rds` (with `Race`, `Race_Ethnicity`, `Sex_male`, `Age`, wide-pivoted diagnosis flags, and per-drug `*_Index` / `*_Use` columns)
   - `treatment_overlap_table.rds`, `dte_cohort_data.rds` (cohort joined to `mdd_data` + overlap, with per-drug `*_mdd_to_index_days` / `*_age_at_index_years`)
   - `nonswitch_periods.rds` (with `tfe_at_index_bgn` / `tfe_at_index_end`)
   - `psych_proc.rds` (joined to CPT acuity), `encounter_table.rds`

2. **`get_TRD.R`** — Identifies TRD patients from antidepressant consecutive periods, writing CSV outputs (`antidepressant_consecutive_instance.csv`, `...period.csv`, `...period_tab_summ.csv`, `...period_maxDrugs.csv`, `IDs-TRD.csv`).

3. **`get_Antidepressant_Treatment_Timeline.R`** / **`get_Hydrochlorothiazide_Treatment_Timeline.R`** — Build consecutive instance + period RDS records (`antidepressant_antipsychotic_consecutive_instance.rds`, `...period.rds`, `hydrochlorothiazide_consecutive_instance.rds`).

4. **`get_Nontreatment_Timelines.R`** — Adds the nontreatment arm. Samples the time-from-eligibility distribution of target-drug patients (with `set.seed(2025)`) to emulate index dates for non-switch candidates, runs KS diagnostics, and writes `dte_cohort_wNontreat_data.rds` (primary cohort used downstream) plus `nontreatment_timelines_result-{target_drug}.rds`. Renders `report_Nontreatment_Timelines` and `report_Treatment_Overlap`.

5. **`get_Diagnosis_Timeline.R`** — Appends diagnosis timeline flag variables → `data_DTE_DiagnosisTimelineVars.rds`. Renders `report_Eligibility_Criteria` and `report_Propensity_Covariates`.

6. **`analysis_Propensity_Scoring.R`** — Looped over every `comparator_groups` entry (= `nontreatment_group` + `comparator_drugs`). Writes `PS_Covariates-{group}.csv`, `PS_Matched_Dataset-{group}.rds`, `PS_Weighted_Dataset-{group}.rds`, `propensity_scoring_result-{target_drug}Vs{group}.rds`, and renders `report_Propensity_Scoring-{target_drug}Vs{group}.html`. Follows with `report_PS_Covariate_Summary`.

7. **Outcome scripts** run in parallel via `doParallel` (`detectCores() - 1` workers). Outputs are target-drug-suffixed:
   - `get_Outcomes_PsychProc.R` → `outcomes_psych-{target_drug}.rds`
   - `get_Outcomes_Visits.R` → `outcomes_visits-{target_drug}.rds` (currently excluded from the downstream join in `main.R`)
   - `get_Outcomes_MedChanges.R` → `outcomes_med_changes-{target_drug}.rds`
   - `get_Outcomes_HCMedChanges.R` → `outcomes_hc_med_changes-{target_drug}.rds`
   The three joined outcomes are pivoted to long format and saved as `all_outcomes-{target_drug}.rds`.

8. **Analysis** — Loops over `comparator_groups × analyses`, with a **single hardcoded period** `"15 days-12 months after index"` (not every period in `period_info.csv`):
   - `analysis_Negative_Binomial_Regression.R` — NB GLM IRR for `n_psych_days` and `n_med_changes`. Writes `nb_result-*.rds`, renders `report_NB-*.html` per analysis and a final `report_NB_Summary-{target_drug}.html`.
   - `analysis_PWP_Gap_Time_Cox_Model.R` — PWP gap-time Cox HR for `psych_visits` and `med_changes`. Writes `pwp_result-*.rds`, renders `report_PWP-*.html` per analysis and a final `report_PWP_Summary-{target_drug}.html`.

`analysis_Sensitivity_Med_Changes.R` and `analysis_Sensitivity_Med_Changes-Change_Type.R` exist as standalone scripts **but are not invoked by `main.R`**; run them manually if you need those sensitivity analyses.

## Key Conventions

**Script header pattern:**
```r
library(tidyverse)
library(jsonlite)
source("helper_functions.R")
config <- fromJSON("config.json")
```

**Function pattern:** `get_*()` and `analysis_*()` functions save results to disk via `save()` or `write.csv()` and `return(invisible())`. Most accept an `overwrite` flag (default `TRUE`) for conditional recomputation.

**RDS payload:** `load("file.rds")` restores the original object name — check the creating script. For example, `dte_cohort_wNontreat_data.rds` restores as `this.data` (see `get_Nontreatment_Timelines.R`), not `dte_cohort_data`.

**Consecutive instances:** A gap > 180 days between prescriptions of the same `SimpleGenericName` starts a new `consecutive_instance`. Used in TRD identification and treatment timeline scripts.

**Study cohort label:** `"{target_drug} vs {comparator_group}"` (e.g., `"Semaglutide vs DPP4i"`).

**Outcome periods** (keys as used in code — `period_info.csv`):
- `6-0 months before index` (−180 to 0, baseline)
- `15 days-3 months after index` (15 to 90)
- `15 days-6 months after index` (15 to 180)
- `15 days-12 months after index` (15 to 360, primary analysis period)
- `6-12 months after index` (180 to 360)

**Treatment variable:** `treatment = 1` (target drug, Semaglutide), `treatment = 0` (comparator/nontreatment). `treatment_name` carries the string label.

**Patient IDs:** `PatientDurableKey` throughout (CCM medication/diagnosis/encounter tables and cohort assembly). Some older sensitivity scripts use `person_id` for the same patients — join carefully when mixing.

**PS matching:** Caliper = 0.1 × SD(logit PS); acceptable covariate balance threshold = SMD < 0.1.
