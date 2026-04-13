library(rmarkdown)
library(tidyverse)
library(doParallel)
library(corrplot)
library(libRtheme)
library(plotrix)
library(tableone)
library(kableExtra)
library(viridis)
library(caret)
library(twang)
library(MatchIt)
library(ggplot2)
library(ggcorrplot)
library(survey)
library(scales)
library(MASS)
library(performance)
library(DHARMa)
library(sjPlot)
library(jsonlite)

config <- fromJSON("config.json")

data_pull_date                  <- as.Date(config$data_pull_date)
target_drug                     <- config$target_drug
comparator_drugs                <- config$comparator_drugs
nontreatment_group              <- config$nontreatment_group
eligibility_inclusion_diagnoses <- config$eligibility_inclusion_diagnoses

all_drugs         <- c(target_drug, comparator_drugs)
comparator_groups <- c(nontreatment_group, comparator_drugs)
all_groups        <- c(target_drug, comparator_groups)

var_name_to_pretty <- read.csv(config$files$var_name_to_pretty)
ps_covariates      <- read.csv(config$files$ps_covariates)

atc_drugs <- read.csv(config$files$atc_drugs) %>%
  mutate(length = nchar(Name)) %>%
  arrange(Name)

drug_class  <- read.csv(config$files$drug_class)
cpt_acuity  <- read.csv(config$files$cpt_acuity)
period_info <- read.csv(config$files$period_info)

# ── Identify TRD patients ─────────────────────────────────────────────────────----

source("get_TRD.R")

message("Identifying TRD patients")
get_TRD(
  mdd_data                  = mdd_data,
  antidepressant_table      = antidepressant_table,
  instance_filename         = "OutputData/antidepressant_consecutive_instance.csv",
  period_filename           = "OutputData/antidepressant_consecutive_period.csv",
  period_summ_filename      = "OutputData/antidepressant_consecutive_period_tab_summ.csv",
  period_max_drugs_filename = "OutputData/antidepressant_consecutive_period_maxDrugs.csv",
  trd_ids_filename          = "OutputData/IDs-TRD.csv",
  overwrite                 = TRUE
)

# ── Build antidepressant/antipsychotic treatment timelines ────────────────────----

source("get_Antidepressant_Treatment_Timeline.R")

message("Building antidepressant/antipsychotic treatment timelines")
get_Antidepressant_Treatment_Timeline(
  drug_class           = drug_class,
  antidepressant_table = antidepressant_table,
  antipsychotics_table = antipsychotics_table,
  instance_filename    = "OutputData/antidepressant_antipsychotic_consecutive_instance.rds",
  period_filename      = "OutputData/antidepressant_antipsychotic_consecutive_period.rds",
  overwrite            = TRUE
)

# ── Build hydrochlorothiazide treatment timelines ─────────────────────────────----

source("get_Hydrochlorothiazide_Treatment_Timeline.R")

message("Building hydrochlorothiazide treatment timelines")
get_Hydrochlorothiazide_Treatment_Timeline(
  hydrochlorothiazide_table = hydrochlorothiazide_table,
  instance_filename         = "OutputData/hydrochlorothiazide_consecutive_instance.rds",
  overwrite                 = TRUE
)

# ── Build nontreatment cohort ─────────────────────────────────────────────────----

source("get_Antidiabetic_Nontreatment_Timelines.R")

message("Building nontreatment cohort for: ", target_drug)
nontreat_result <- get_Antidiabetic_Nontreatment_Timelines(dte_cohort_data, nonswitch_periods, target_drug, nontreatment_group, mdd_data)

this.data <- nontreat_result$dte_cohort_data2
save(this.data, file = "OutputData/dte_cohort_wNontreat_data.rds")

result_file <- paste0("OutputData/nontreatment_timelines_result-", target_drug, ".rds")

save(nontreat_result, file = result_file)

# ── Render nontreatment timelines report ──────────────────────────────────────----

render(
  input       = "report_Antidiabetic_Nontreatment_Timelines.Rmd",
  output_file = paste0("Reports/report_Antidiabetic_Nontreatment_Timelines-", target_drug, ".html"),
  params      = list(
    target_drug        = target_drug,
    nontreatment_group = nontreatment_group,
    result_file        = result_file
  ),
  envir = new.env()
)

# ── Render antidiabetic overlap report ───────────────────────────────────────----

render(
  input       = "report_Antidiabetic_Overlap.Rmd",
  output_file = paste0("Reports/report_Antidiabetic_Overlap-", target_drug, ".html"),
  params      = list(
    dte_cohort_data2   = nontreat_result$dte_cohort_data2,
    all_groups         = all_groups,
    nontreatment_group = nontreatment_group
  ),
  envir = new.env()
)

# ── Build diagnosis timeline variables ────────────────────────────────────────----

source("get_Diagnosis_Timeline.R")

message("Building diagnosis timeline variables")
get_Diagnosis_Timeline(
  all_drugs        = all_drugs,
  all_diagnoses    = eligibility_inclusion_diagnoses,
  index_dataset    = nontreat_result$dte_cohort_data2,
  output_filename  = "OutputData/data_DTE_DiagnosisTimelineVars.rds"
)

load("OutputData/data_DTE_DiagnosisTimelineVars.rds", verbose = T)
diagnosis_timeline_data <- this.data

# ── Render eligibility criteria report ───────────────────────────────────────----

render(
  input       = "report_Eligibility_Criteria.Rmd",
  output_file = paste0("Reports/report_Eligibility_Criteria-", target_drug, ".html"),
  params      = list(
    eligibility_inclusion_diagnoses = eligibility_inclusion_diagnoses,
    all_drugs                       = all_drugs,
    var_name_to_pretty              = var_name_to_pretty,
    comparator_drugs                = comparator_drugs,
    target_drug                     = target_drug,
    diagnosis_timeline_data         = diagnosis_timeline_data,
    dte_cohort_data2                = nontreat_result$dte_cohort_data2,
    diag_table                      = diag_table,
    nontreatment_group              = nontreatment_group
  ),
  envir = new.env()
)

# ── Render propensity covariates report ──────────────────────────────────────----

render(
  input       = "report_Propensity_Covariates.Rmd",
  output_file = paste0("Reports/report_Propensity_Covariates-", target_drug, ".html"),
  params      = list(
    dte_cohort_data2   = nontreat_result$dte_cohort_data2,
    all_groups         = all_groups,
    target_drug        = target_drug,
    ps_covariates      = ps_covariates,
    var_name_to_pretty = var_name_to_pretty
  ),
  envir = new.env()
)

# ── Run propensity scoring and render reports ─────────────────────────────────----

source("analysis_Propensity_Scoring.R")

for (group in comparator_groups) {
  message("Running propensity scoring for: ", group)
  ps_result <- analysis_Propensity_Scoring(group, target_drug,
                                           "OutputData/dte_cohort_wNontreat_data.rds",
                                           "Data/ps_covariates.csv")
  
  write.csv(ps_result$matchingVars.final,
            paste0("OutputData/PS_Covariates-", group, ".csv"),
            row.names = FALSE)
  
  weighted.data <- ps_result$weighted.data
  save(weighted.data, file = paste0("OutputData/PS_Weighted_Dataset-", group, ".rds"))
  rm(weighted.data)
  
  matched.data <- ps_result$matched.data
  save(matched.data, file = paste0("OutputData/PS_Matched_Dataset-", group, ".rds"))
  rm(matched.data)
  
  result_file <- paste0("OutputData/propensity_scoring_result-", target_drug, "Vs", group, ".rds")
  save(ps_result, file = result_file)
  rm(ps_result)
  
  message("Rendering report for: ", group)
  render(
    input       = "report_Propensity_Scoring.Rmd",
    output_file = paste0("Reports/report_Propensity_Scoring-", target_drug, "Vs", group, ".html"),
    params      = list(
      target_drug     = target_drug,
      comparator_group = group,
      result_file     = result_file
    ),
    envir = new.env()
  )
}

# ── Render PS covariate summary report ───────────────────────────────────────

render(
  input       = "report_PS_Covariate_Summary.Rmd",
  output_file = paste0("Reports/report_PS_Covariate_Summary-", target_drug, ".html"),
  params      = list(
    comparator_groups = comparator_groups,
    ps_covariates     = ps_covariates,
    target_drug       = target_drug,
    output_filename   = paste0("OutputData/PS_Covariates-Summary-", target_drug, ".csv")
  ),
  envir = new.env()
)

# ── Compute outcomes ──────────────────────────────────────────────────────────----

matched_data_files <- setNames(
  paste0("OutputData/PS_Matched_Dataset-", comparator_groups, ".rds"),
  comparator_groups
)

# visits_file      <- "OutputData/dte_cohort_visits.rds"
med_changes_file <- "OutputData/antidepressant_antipsychotic_consecutive_period.rds"
hc_med_file      <- "OutputData/hydrochlorothiazide_consecutive_instance.rds"
psych_proc_file  <- "OutputData/psych_proc.rds"

outcomes_files <- list(
  psych   = paste0("OutputData/outcomes_psych-",         target_drug, ".rds"),
# visits  = paste0("OutputData/outcomes_visits-",        target_drug, ".rds"),
  med     = paste0("OutputData/outcomes_med_changes-",   target_drug, ".rds"),
  hc_med  = paste0("OutputData/outcomes_hc_med_changes-",target_drug, ".rds")
)

source("get_Outcomes_PsychProc.R")
# source("get_Outcomes_Visits.R")
source("get_Outcomes_MedChanges.R")
source("get_Outcomes_HCMedChanges.R")

outcome_tasks <- list(
  list(fn = get_Outcomes_PsychProc,   src_file = psych_proc_file,   out_file = outcomes_files$psych),
# list(fn = get_Outcomes_Visits,      src_file = visits_file,       out_file = outcomes_files$visits),
  list(fn = get_Outcomes_MedChanges,  src_file = med_changes_file,  out_file = outcomes_files$med),
  list(fn = get_Outcomes_HCMedChanges,src_file = hc_med_file,       out_file = outcomes_files$hc_med)
)

n_workers <- min(length(outcome_tasks), max(1L, detectCores(logical = TRUE) - 1L))
cl <- makeCluster(n_workers)
registerDoParallel(cl)

message("Running outcome computations in parallel (", n_workers, " workers)...")
foreach(
  task      = outcome_tasks,
  .packages = c("dplyr", "tidyr", "lubridate", "readr"),
  .export   = c("matched_data_files", "period_info", "target_drug",
                "comparator_groups",
                "get_Outcomes_PsychProc",
              # "get_Outcomes_Visits",
                "get_Outcomes_MedChanges",
                "get_Outcomes_HCMedChanges")
) %dopar% {
  task$fn(task$src_file, matched_data_files, period_info,
          target_drug, comparator_groups,
          task$out_file)
}

stopCluster(cl)
message("All outcome computations complete.")

# ── Join outcome tables and pivot to long format ──────────────────────────────----

load(outcomes_files$psych)
outcomes_psych <- outcomes

# load(outcomes_files$visits)
# outcomes_visits <- outcomes

load(outcomes_files$med)
outcomes_med_changes <- outcomes

load(outcomes_files$hc_med)
outcomes_hc_med_changes <- outcomes

all_outcomes_wide <- outcomes_psych %>%
  # left_join(outcomes_visits,
  #           by = c("PatientDurableKey", "study_cohort", "period")) %>%
  left_join(outcomes_med_changes,
            by = c("PatientDurableKey", "study_cohort", "period")) %>%
  left_join(outcomes_hc_med_changes,
            by = c("PatientDurableKey", "study_cohort", "period"))

all_outcomes <- all_outcomes_wide %>%
  pivot_longer(4:ncol(.), names_to = "var_name", values_to = "value") %>%
  mutate(
    period       = factor(period,       levels = period_info$period),
    study_cohort = factor(study_cohort, levels = paste0(target_drug, " vs ", comparator_groups)),
    var_name     = factor(var_name)
  )

save(all_outcomes, file = paste0("OutputData/all_outcomes-", target_drug, ".rds"))

# ── Negative Binomial Regression analyses ─────────────────────────────────────----

source("analysis_Negative_Binomial_Regression.R")
load(paste0("OutputData/all_outcomes-", target_drug, ".rds"))

matched_data_files <- setNames(
  paste0("OutputData/PS_Matched_Dataset-", comparator_groups, ".rds"),
  comparator_groups
)

med_changes_file <- "OutputData/antidepressant_antipsychotic_consecutive_period.rds"
hc_med_file      <- "OutputData/hydrochlorothiazide_consecutive_instance.rds"
psych_proc_file  <- "OutputData/psych_proc.rds"

nb_analyses <- list(
  list(dep_var    = "n_psych_days",
       covariates = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years")),
  list(dep_var    = "n_med_changes",
       covariates = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years"))
)

nb_period_name <- "15 days-12 months after index"

dir.create("Reports", showWarnings = FALSE)

nb_result_files <- character(0)

for (group in comparator_groups) {
  for (analysis in nb_analyses) {
    result_file <- paste0(
      "OutputData/nb_result-", target_drug, "Vs", group,
      "-", analysis$dep_var, "-period", nb_period_name, ".rds"
    )

    message("Fitting NB model: ", target_drug, " vs ", group,
            " | ", analysis$dep_var, " | period ", nb_period_name)

    analysis_Negative_Binomial_Regression(
      matched_data_file = matched_data_files[[group]],
      all_outcomes      = all_outcomes,
      comparator_group  = group,
      target_drug       = target_drug,
      period_name       = nb_period_name,
      dep_var           = analysis$dep_var,
      covariates        = analysis$covariates,
      output_file       = result_file
    )

    nb_result_files <- c(nb_result_files, result_file)

    render(
      input       = "report_Negative_Binomial_Regression.Rmd",
      output_file = paste0("Reports/report_NB-", target_drug, "Vs", group,
                           "-", analysis$dep_var, ".html"),
      params = list(
        result_file      = result_file,
        target_drug      = target_drug,
        comparator_group = group,
        dep_var          = analysis$dep_var
      ),
      envir = new.env()
    )
  }
}

render(
  input       = "report_NB_Summary.Rmd",
  output_file = paste0("Reports/report_NB_Summary-", target_drug, ".html"),
  params = list(
    result_files = nb_result_files,
    target_drug  = target_drug
  ),
  envir = new.env()
)

# ── PWP Gap Time Cox Model analyses ───────────────────────────────────────────----

source("analysis_PWP_Gap_Time_Cox_Model.R")

matched_data_files <- setNames(
  paste0("OutputData/PS_Matched_Dataset-", comparator_groups, ".rds"),
  comparator_groups
)

med_changes_file <- "OutputData/antidepressant_antipsychotic_consecutive_period.rds"
hc_med_file      <- "OutputData/hydrochlorothiazide_consecutive_instance.rds"
psych_proc_file  <- "OutputData/psych_proc.rds"

pwp_period_name <- "15 days-12 months after index"
pwp_period_row  <- period_info[period_info$period == pwp_period_name, ]
pwp_bgn_win     <- pwp_period_row$bgn_win
pwp_end_win     <- pwp_period_row$end_win

pwp_analyses <- list(
  list(dep_var        = "psych_visits",
       event_data_file= psych_proc_file,
       event_date_col = "OutcomeDate",
       dedup_by_day   = TRUE,
       covariates     = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years")),
  list(dep_var        = "med_changes",
       event_data_file= med_changes_file,
       event_date_col = "first_record",
       dedup_by_day   = TRUE,
       covariates     = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years"))
)

pwp_result_files <- character(0)

for (group in comparator_groups) {
  for (analysis in pwp_analyses) {
    result_file <- paste0(
      "OutputData/pwp_result-", target_drug, "Vs", group,
      "-", analysis$dep_var, "-period", pwp_period_name, ".rds"
    )

    message("Fitting PWP model: ", target_drug, " vs ", group,
            " | ", analysis$dep_var, " | period ", pwp_period_name)

    analysis_PWP_Gap_Time_Cox_Model(
      matched_data_file = matched_data_files[[group]],
      event_data_file   = analysis$event_data_file,
      event_date_col    = analysis$event_date_col,
      dedup_by_day      = analysis$dedup_by_day,
      comparator_group  = group,
      target_drug       = target_drug,
      period_name       = pwp_period_name,
      bgn_win           = pwp_bgn_win,
      end_win           = pwp_end_win,
      dep_var           = analysis$dep_var,
      covariates        = analysis$covariates,
      output_file       = result_file
    )

    pwp_result_files <- c(pwp_result_files, result_file)

    render(
      input       = "report_PWP_Gap_Time_Cox_Model.Rmd",
      output_file = paste0("Reports/report_PWP-", target_drug, "Vs", group,
                           "-", analysis$dep_var, ".html"),
      params = list(
        result_file      = result_file,
        target_drug      = target_drug,
        comparator_group = group,
        dep_var          = analysis$dep_var
      ),
      envir = new.env()
    )
  }
}

render(
  input       = "report_PWP_Summary.Rmd",
  output_file = paste0("Reports/report_PWP_Summary-", target_drug, ".html"),
  params = list(
    result_files = pwp_result_files,
    target_drug  = target_drug
  ),
  envir = new.env()
)

