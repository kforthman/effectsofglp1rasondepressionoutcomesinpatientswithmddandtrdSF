# Generalized DTE Pipeline

An R-based analytic pipeline for studying depression outcomes under different drug regimens in patients with Major Depressive Disorder (MDD). The pipeline is generalized: the target drug, comparator drug classes, eligibility criteria, and outcome time windows are all driven by `config.json`, so the same code can evaluate any treatment of interest against one or more comparators (including a nontreatment arm). The current configuration studies GLP-1 receptor agonists (semaglutide) against seven antidiabetic comparator classes, with treatment-resistant depression (TRD) as a subgroup of interest.

## Running the Pipeline

[`main.R`](main.R) is the **central script for running the analytic pipeline**. It orchestrates every stage end-to-end: data ingestion, cohort construction, nontreatment arm emulation, propensity-score matching/weighting, parallelized outcome computation, and the final regression analyses and reports.

```bash
Rscript -e 'renv::restore()'   # first-time dependency install (uses renv.lock)
Rscript main.R                 # runs the full pipeline
```

`.Rprofile` sources `renv/activate.R`, so R sessions in this directory automatically use the project library. Individual `get_*.R` and `analysis_*.R` scripts can be run standalone but depend on upstream `OutputData/*.rds` artifacts, which `main.R` creates in order.

Intermediate data lands in `OutputData/`; rendered HTML reports land in `Reports/`; formatted Table 1 HTML lands in `html_tables/`. All three directories are auto-created by `main.R` on first run.

## Configuration

All parameterization lives in `config.json`. The file is git-ignored; a template is tracked as [`config-example.json`](config-example.json). Copy it to `config.json` and edit before running.

**Top-level keys:**

| Key | Purpose |
|---|---|
| `data_pull_date` | EHR snapshot date (`YYYY-MM-DD`). Used to compute `Age` at snapshot in `mdd_data`. |
| `target_drug` | The treatment drug under study (e.g. `"Semaglutide"`). Every analysis script compares this against each comparator. |
| `comparator_drugs` | Array of comparator drug class labels (e.g. `["DPP4i", "GLP1RA", "Insulins", "Metformin", "SGLT2i", "SU", "TZD"]`). Analyses loop over each one. |
| `nontreatment_group` | Label for the synthetic no-treatment arm (e.g. `"Nontreatment"`). Index dates for this arm are emulated by `get_Nontreatment_Timelines.R`. |
| `eligibility_inclusion_diagnoses` | Array of diagnosis keys used to build wide-pivoted diagnosis flags on `mdd_data` and drive eligibility reporting. |
| `database` | ODBC connection block (`driver`, `server`, `database`, `trusted_connection`, `timeout`). When present, `prepData.R` reads source tables from SQL Server; when absent, it reads each `files.*` entry as a CSV path instead. |
| `files` | Maps logical table names to file paths or SQL object names. See below. |

**`files` block.** Eight entries point at reference CSVs in `Data/` (static, checked in). Eight more identify the source tables the pipeline ingests — these values are SQL object names like `dbo.MedicationTable` when the `database` block is present, or paths to CSVs when it is absent:

Reference files: `var_name_to_pretty`, `ps_covariates`, `atc_drugs`, `drug_class`, `cpt_acuity`, `period_info`, `column_schema`, `medication_recode`.

Source tables: `med_table`, `diag_table`, `mdd_data`, `treatment_overlap_table`, `dte_cohort_data`, `nonswitch_periods`, `psych_proc`, `encounter_table`.

Every source table is schema-validated against `Data/column_schema.csv` on load (`check_schema_table()` in `helper_functions.R`); a column mismatch halts the pipeline.

## Reference Data Files (`Data/`)

All files in `Data/` are tracked in git and represent curated, reusable inputs that define the analytic universe (covariate sets, drug dictionaries, outcome windows, labels). None of them contain patient data.

| File | Rows | Contents |
|---|---:|---|
| [`column_schema.csv`](Data/column_schema.csv) | 184 | Expected columns, R types, and date formats for each of the 8 source tables (`med_table`, `diag_table`, `mdd_data`, `treatment_overlap_table`, `dte_cohort_data`, `nonswitch_periods`, `psych_proc`, `encounter_table`). Used by `check_schema_table()` to validate DB/CSV inputs and by `make_col_types()` / `apply_col_types()` to coerce types on read. |
| [`medication_recode.csv`](Data/medication_recode.csv) | 273 | Canonical medication-name map. Columns: `table`, `raw_name`, `canonical_name`, `subclass`. Keyed by the logical table being recoded (`antidepressant`, `antipsychotics`, `hydrochlorothiazide`, `treatments`). `raw_name = "remove"` in `canonical_name` drops entries out of scope. `subclass` overrides `PharmaceuticalSubclass` in the treatments table so combination products map to the intended drug class. |
| [`Drug_ATC_Categories.csv`](Data/Drug_ATC_Categories.csv) | 385 | ATC-coded drug dictionary. Columns: `ATC_code`, `Name`, and four `Category_Level_*` columns. `prepData.R` joins this against recoded `SimpleGenericName` and filters by ATC prefix: antidepressants to `N06A*`; antipsychotics to `N05AE`, `N05AH` (except `N05AH02`), `N05AL`, `N05AN`, `N05AX`; hydrochlorothiazide to `C03AA03`. |
| [`DrugClasses.csv`](Data/DrugClasses.csv) | 41 | Hand-curated map from drug name to antidepressant class (`SSRI`, `SNRI`, `TCA`, `MAOI`, `Atypical`, `Other`). Consumed by `get_Antidepressant_Treatment_Timeline.R`. |
| [`period_info.csv`](Data/period_info.csv) | 5 | Outcome time windows relative to each patient's index date, in days. Columns: `period`, `bgn_win`, `end_win`. The five defined periods are `6-0 months before index` (−180 to 0, baseline), `15 days-3 months after index` (15 to 90), `15 days-6 months after index` (15 to 180), `15 days-12 months after index` (15 to 360, the primary analysis period), and `6-12 months after index` (180 to 360). Outcome-count scripts loop over this table; `main.R` uses the 12-month period for the primary NB and PWP models. |
| [`ps_covariates.csv`](Data/ps_covariates.csv) | 36 | Propensity-score covariate list driving `analysis_Propensity_Scoring.R`. Columns: `var`, `var_type` (`continuous` or `binary`). Intended to be customized per DTE — edit this file to change the set of covariates included in the propensity model. |
| [`psych_proc_tiers-manualEdit-MPP.csv`](Data/psych_proc_tiers-manualEdit-MPP.csv) | 56 | CPT code acuity tiers for psychiatric procedures. Columns: `source_concept_id`, `source_concept_code` (CPT), `source_vocabulary`, `source_concept_name`, `count_records`, `level`. `level` (manually assigned) ranks the acuity of each procedure; joined onto `psych_proc` in `prepData.R` and used by `get_Outcomes_PsychProc.R`. |
| [`var_name_to_pretty.csv`](Data/var_name_to_pretty.csv) | 52 | Display-label dictionary for reports. Columns: `var_name`, `pretty_name`, `pretty_var_category`. Maps the code-side variable names to human-readable labels and groups them into categories (e.g. `Antidiabetics`, `Demographics`) used by the Rmd reports. |

## Related Documents

- [`CLAUDE.md`](CLAUDE.md) — deep implementation notes, pipeline architecture, helper-function reference, and key conventions. Start here when editing the code.
- [`Data_Diary/prepData-schema.xlsx`](Data_Diary/prepData-schema.xlsx) — intended schema of the tables **after** processing by `prepData.R` (i.e. the shape of the `OutputData/*.rds` artifacts consumed by downstream scripts).
- [`pipeline_map.csv`](pipeline_map.csv) / [`file_order.csv`](file_order.csv) — script dependency DAG generated by [`generate_pipeline_map.py`](generate_pipeline_map.py).
