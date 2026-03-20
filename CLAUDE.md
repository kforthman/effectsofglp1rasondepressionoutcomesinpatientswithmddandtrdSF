# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based epidemiological study analyzing the effects of GLP-1 receptor agonists (semaglutide) on depression outcomes in patients with MDD and TRD. The project runs on the **NIH All of Us Research Program Workbench** (Terra/JupyterLab environment) and queries patient data from **BigQuery** (the All of Us Curated Data Repository).

## Running Scripts

Scripts are R files with Jupyter-style `# %%` cell markers. They are run interactively on the All of Us Workbench Jupyter environment. To run a script:

```bash
Rscript <script_name>.R
```

To regenerate pipeline metadata after modifying scripts:

```bash
python generate_pipeline_map.py
```

This rewrites `pipeline_map.csv` (per-script I/O dependencies) and `file_order.csv` / `file_order.html` (topological execution order).

## Environment & Key Variables

All scripts depend on these environment variables (set automatically on the workbench):

- `WORKSPACE_BUCKET` — Google Cloud Storage bucket for intermediate `.rds`/`.csv` files
- `WORKSPACE_CDR` — BigQuery dataset path for the All of Us CDR
- `GOOGLE_PROJECT` — GCP billing project
- `OWNER_EMAIL` — Workspace owner email (used for GCS auth and file paths)

## Shared Utility Files (sourced at top of every script)

Every script begins with:
```r
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()
```

- **`workbench_functions.txt`** — Core utilities: `notebook_setup()` (installs/loads all packages, sets options), `grab_data()` / `download_data()` (BigQuery helpers), `stat_smd()` / `stat_pval()` (SMD and p-value stats for balance checking)
- **`easy_delete_copy_R.txt`** — File management: `delete_if_exists_local()`, `gs_exists()`, `copy_to_bucket()`, `download_nonexist_data()`
- **`data_prep_functions.txt`** — ETL helpers: `read_bq_export_from_workspace_bucket()`, `prep_save_condition_table()`, `prep_save_drug_table()`, `prep_save_visit_table()`, etc., `translate_table()` (applies drug name translation tables)
- **`my_table1.txt`** — Table 1 formatting utilities

## Pipeline Architecture

Scripts have a strict dependency order (see `file_order.csv`). The high-level stages are:

### Stage 1 — Concept extraction (`get_All_Concepts.R`)
Queries BigQuery for all clinical concepts (conditions, drugs, visits, measurements). Saves standardized `.rds` files to `Data_Prepped/` with naming convention:
- `Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds`
- `Data_Prepped/IDs-{concept_name}-{table_type}.txt` (participant ID lists)

### Stage 2 — Ancillary data preparation (`get_EHR_Availability_Timeframes_noObs_v2.R`, `get_BMI.R`, `get_TRD.R`, `get_Diagnoses.R`, `get_Antidiabetic_Drug_Exposure.R`, `get_Antidiabetic_Timelines_v2.R`, `get_Diagnosis_Timeline.R`, `get_Visit_Vars.R`, `get_zip.R`, `prep_Participant_survey_answers.R`)
Each script loads raw `Data_Prepped/` files and produces intermediate `.rds` files used downstream (e.g., `ehr_length_noObs_v2.rds`, `data_DTE_diagnoses.rds`).

### Stage 3 — DTE cohort assembly (`get_DTE_cohort.R`)
Applies eligibility criteria and assembles the Drug Treatment Episode cohort → `dte_cohort_data.rds`.

### Stage 4 — Nontreatment timelines (`get_Antidiabetic_Nontreatment_Timelines_v3.R`)
Adds nontreatment comparison arm → `dte_cohort_wNontreat_data.rds`. This is the primary cohort dataset used by most downstream scripts.

### Stage 5 — Propensity scoring (`analysis_Propensity_Scoring_SemaglutideVs*.R`)
One script per comparator drug class: DPP4i, GLP1RA, Insulins, Metformin, Nontreatment, SGLT2i, SU, TZD. Each reads `dte_cohort_wNontreat_data.rds` and `ps_covariates.csv`, and produces:
- `PS_Matched_Dataset-{drug}.rds` — 1:1 propensity-matched cohort
- `PS_Weighted_Dataset-{drug}.rds` — IPTW-weighted cohort
- `PS_Covariates-{drug}.csv` — covariate balance summary

### Stage 6 — Treatment timelines and outcomes
- `get_Drug_Translation_Tables_v3.R` → translated antidepressant/antipsychotic drug records
- `get_Antidepressant_Treatment_Timeline_v2.R` → `antidepressant_antipsychotic_consecutive_period.rds`
- `get_DTE_visits.R` → `dte_cohort_psych_visits.rds`, `dte_cohort_visits.rds`
- `get_Hydrochlorothiazide_Treatment_Timeline.R` → active comparator for sensitivity analyses
- `get_Outcomes_v3.R` → `all_outcomes.rds`

### Stage 7 — Statistical analyses
- `analysis_Negative_Binomial_Regression.R` — count outcomes (healthcare utilization)
- `analysis_PWP_Gap_Time_Cox_Model_v2.R` — Prentice-Williams-Peterson gap time Cox model for recurrent events (psychiatric visits)
- `analysis_Sensitivity_Med_Changes.R` / `analysis_Sensitivity_Med_Changes-Change_Type.R` — sensitivity analyses

### Exploratory and demonstration scripts
`explore_*.R` scripts are for EDA and do not produce files used by downstream analysis. `demonstration_*.R` scripts are standalone debugging examples.

## Key Data Conventions

- Intermediate data is persisted to the local Jupyter disk as `.rds` (R binary) or `.csv` files; large exports live in `$WORKSPACE_BUCKET` on GCS.
- `Data_Prepped/` contains all concept-level preprocessed data.
- The drug treatment comparison pairs are always **Semaglutide vs {comparator}**; the `treatment` variable is 1=Semaglutide, 0=comparator.
- Index date logic: for drug comparators, index = first dispense date; for "Nontreatment", index = `Nontreatment_index`.
- Loaded `.rds` files consistently assign their payload to `this.data` (or a named variable immediately after `load()`).
