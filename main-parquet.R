library(rmarkdown)
library(tidyverse)
library(doParallel)
library(corrplot)
source("../libr.R")
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
# library(DHARMa)
library(sjPlot)
library(jsonlite)
library(lubridate)
library(patchwork)
library(arrow)
library(dplyr)
source("../CBPSlite.R")

source("helper_functions.R")

config <- fromJSON("config-FullSample.json")

data_pull_date                  <- as.Date(config$data_pull_date)
target_drug                     <- config$target_drug
comparator_drugs                <- config$comparator_drugs
nontreatment_group              <- config$nontreatment_group
eligibility_inclusion_diagnoses <- config$eligibility_inclusion_diagnoses

all_drugs         <- c(target_drug, comparator_drugs)
comparator_groups <- c(nontreatment_group, comparator_drugs)
all_groups        <- c(target_drug, comparator_groups)

var_name_to_pretty <- read.csv(config$files$var_name_to_pretty)
comp_vars          <- read.csv(config$files$comp_vars)$var_name
ps_covariates      <- read.csv(config$files$ps_covariates)

atc_drugs <- read_csv(config$files$atc_drugs, 
                      col_types = cols(
                        ATC_code         = readr::col_factor(),
                        Name             = readr::col_character(),
                        Category_Level_4 = readr::col_factor(),
                        Category_Level_3 = readr::col_factor(),
                        Category_Level_2 = readr::col_factor(),
                        Category_Level_1 = readr::col_factor()
                      )) %>%
  mutate(length = nchar(Name)) %>%
  arrange(Name) %>%
  mutate(Name = as.factor(Name))

drug_class           <- read.csv(config$files$drug_class)
cpt_acuity           <- read.csv(config$files$cpt_acuity)
period_info          <- read.csv(config$files$period_info)
n_patient_partitions <- config$n_patient_partitions
overwrite            <- as.logical(config$overwrite)

if(!dir.exists("OutputData")){
  dir.create("OutputData")
}
if(!dir.exists("Parquet_batched_OutputData")){
  dir.create("Parquet_batched_OutputData")
}
if(!dir.exists("Reports")){
  dir.create("Reports")
}
if(!dir.exists("html_tables")){
  dir.create("html_tables")
}

# -- Prep Data ---------------------------------------------------------
# source("prepData-parquet.R")

# -- Identify TRD patients ---------------------------------------------------------

source("get_TRD.R")

message("Identifying TRD patients")
for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  mdd_data <- open_dataset("Parquet_batched_prepped/mdd_data") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  med_table_ad <- open_dataset("Parquet_batched_prepped/med_table_ad") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  this_result <- get_TRD(
    mdd_data                  = mdd_data,
    antidepressant_table      = med_table_ad
  )
  
  write_dataset(
    this_result$consecutive_instance_tab %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/antidepressant_consecutive_instance",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  write_dataset(
    this_result$consecutive_period_tab %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/antidepressant_consecutive_period",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  write_dataset(
    this_result$consecutive_period_tab_summ %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/antidepressant_consecutive_period_tab_summ",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  write_dataset(
    this_result$consecutive_period_maxDrugs %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/antidepressant_consecutive_period_maxDrugs",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  write_dataset(
    this_result$TRD_list_df %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/IDs-TRD",
    format = "parquet",
    partitioning = "batch_number"
  )
  
}

# -- Build antidepressant/antipsychotic treatment timelines ------------------------

source("get_Antidepressant_Treatment_Timeline.R")

message("Building antidepressant/antipsychotic treatment timelines")

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  antidepressant_table <- open_dataset("Parquet_batched_prepped/med_table_ad") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  antipsychotics_table <- open_dataset("Parquet_batched_prepped/med_table_ap") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  this_result <- get_Antidepressant_Treatment_Timeline(
    drug_class           = drug_class,
    antidepressant_table = antidepressant_table,
    antipsychotics_table = antipsychotics_table
  )
  
  write_dataset(
    this_result$antidepressant_antipsychotic_consecutive_instance %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/antidepressant_antipsychotic_consecutive_instance",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  write_dataset(
    this_result$antidepressant_antipsychotic_consecutive_period %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/antidepressant_antipsychotic_consecutive_period",
    format = "parquet",
    partitioning = "batch_number"
  )
}

# -- Build hydrochlorothiazide treatment timelines ---------------------------------

source("get_Hydrochlorothiazide_Treatment_Timeline.R")

message("Building hydrochlorothiazide treatment timelines")

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  hydrochlorothiazide_table <- open_dataset("Parquet_batched_prepped/med_table_hctz") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  this_result <- get_Hydrochlorothiazide_Treatment_Timeline(
    hydrochlorothiazide_table = hydrochlorothiazide_table
  )
  
  write_dataset(
    this_result$hydrochlorothiazide_consecutive_instance %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/hydrochlorothiazide_consecutive_instance",
    format = "parquet",
    partitioning = "batch_number"
  )
  
}

# -- Build nontreatment cohort -----------------------------------------------------

source("get_Nontreatment_Timelines.R")

message("Building nontreatment cohort for: ", target_drug)

col_TimelineCriteria <- paste0(target_drug,        "_meets_timeline_criteria")
col_mdd_to_index     <- paste0(target_drug,        "_mdd_to_index_days")

tfe_dist <- open_dataset("Parquet_batched_prepped/dte_cohort_data") %>%
  filter(!!sym(col_TimelineCriteria) == 1) %>%
  count(!!sym(col_mdd_to_index)) %>%
  collect() %>%
  rename(tfe_at_index_days = !!sym(col_mdd_to_index)) %>%
  mutate(freq = n / sum(n))

save(tfe_dist, file = paste0("OutputData/", target_drug, "_tfe_dist.rds"))

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  dte_cohort_data <- open_dataset("Parquet_batched_prepped/dte_cohort_data") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  nonswitch_periods <- open_dataset("Parquet_batched_prepped/nonswitch_periods") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  mdd_data <- open_dataset("Parquet_batched_prepped/mdd_data") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  this_result <- get_Nontreatment_Timelines(
    dte_cohort_data        = dte_cohort_data,
    nonswitch_periods      = nonswitch_periods,
    target_drug            = target_drug,
    nontreatment_group     = nontreatment_group,
    mdd_data               = mdd_data,
    tfe_dist               = tfe_dist
  )
  
  write_dataset(
    this_result$dte_cohort_data2 %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/dte_cohort_wNontreat_data",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  write_dataset(
    this_result$dte_cohort_data3 %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/dte_cohort_wNontreat_data_reporting",
    format = "parquet",
    partitioning = "batch_number"
  )
  
}


# # -- Render treatment overlap report -------------------------------------------
# 
# render(
#   input       = "report_Treatment_Overlap.Rmd",
#   output_file = paste0("Reports/report_Treatment_Overlap.html"),
#   params      = list(
#     nontreat_data_filename = "OutputData/dte_cohort_wNontreat_data.rds",
#     all_groups         = all_groups,
#     nontreatment_group = nontreatment_group
#   ),
#   envir = new.env()
# )
# gc()

# -- Build diagnosis timeline variables --------------------------------------------

source("get_Diagnosis_Timeline.R")

message("Building diagnosis timeline variables")

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  nontreat_data <- open_dataset("Parquet_batched_OutputData/dte_cohort_wNontreat_data") %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  this_result <- get_Diagnosis_Timeline(
    all_groups       = all_groups,
    all_diagnoses    = eligibility_inclusion_diagnoses,
    index_dataset    = nontreat_data
  )
  
  write_dataset(
    this_result$diagnosis_timeline_data %>%
      mutate(batch_number = batch_num),
    path = "Parquet_batched_OutputData/data_DTE_DiagnosisTimelineVars",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  gc()
}

# # -- Render propensity covariates report ------------------------------------------
# 
# render(
#   input       = "report_Propensity_Covariates.Rmd",
#   output_file = paste0("Reports/report_Propensity_Covariates-", target_drug, ".html"),
#   params      = list(
#     nontreat_data_filename   = "OutputData/dte_cohort_wNontreat_data.rds",
#     all_groups         = all_groups,
#     target_drug        = target_drug,
#     ps_covariates      = ps_covariates,
#     var_name_to_pretty = var_name_to_pretty
#   ),
#   envir = new.env()
# )
# gc()
# 
# # -- Run propensity scoring and render reports -------------------------------------
# 
# source("analysis_Propensity_Scoring.R")
# 
# for (group in comparator_groups) {
#   message("Running propensity scoring for: ", group)
#   ps_result <- analysis_Propensity_Scoring(comparator_group = group, 
#                                            target_drug = target_drug,
#                                            cohort_file = "OutputData/dte_cohort_wNontreat_data.rds",
#                                            covariates_file = "Data/ps_covariates.csv")
#   
#   write.csv(ps_result$matchingVars.final,
#             paste0("OutputData/PS_Covariates-", group, ".csv"),
#             row.names = FALSE)
#   gc()
#   
#   weighted.data <- ps_result$weighted.data
#   save(weighted.data, file = paste0("OutputData/PS_Weighted_Dataset-", group, ".rds"))
#   rm(weighted.data)
#   gc()
#   
#   matched.data <- ps_result$matched.data
#   save(matched.data, file = paste0("OutputData/PS_Matched_Dataset-", group, ".rds"))
#   rm(matched.data)
#   gc()
#   
#   result_file <- paste0("OutputData/propensity_scoring_result-", target_drug, "Vs", group, ".rds")
#   save(ps_result, file = result_file)
#   rm(ps_result)
#   gc()
#   
#   message("Rendering report for: ", group)
#   render(
#     input       = "report_Propensity_Scoring.Rmd",
#     output_file = paste0("Reports/report_Propensity_Scoring-", target_drug, "Vs", group, ".html"),
#     params      = list(
#       target_drug     = target_drug,
#       comparator_group = group,
#       result_file     = result_file
#     ),
#     envir = new.env()
#   )
#   gc()
# }
# gc()
# 
# # -- Render PS covariate summary report ---------------------------------------
# 
# render(
#   input       = "report_PS_Covariate_Summary.Rmd",
#   output_file = paste0("Reports/report_PS_Covariate_Summary-", target_drug, ".html"),
#   params      = list(
#     comparator_groups = comparator_groups,
#     ps_covariates     = ps_covariates,
#     target_drug       = target_drug,
#     output_filename   = paste0("OutputData/PS_Covariates-Summary-", target_drug, ".csv")
#   ),
#   envir = new.env()
# )
# gc()

# -- Create un-matched dataset -----------------------------------------------------

message("Building unmatched datasets")

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  for(this_comparator in comparator_groups){
    
    target_pop_colname     <- paste0(target_drug, "_Population_for_", target_drug, "_vs_", this_comparator)
    comparator_pop_colname <- paste0(this_comparator, "_Population_for_", target_drug, "_vs_", this_comparator)
    
    varname_target_age_at_index_years     <- paste0(target_drug, "_age_at_index_years")
    varname_comparator_age_at_index_years <- paste0(this_comparator, "_age_at_index_years")
    
    varname_target_mdd_to_index_days     <- paste0(target_drug, "_mdd_to_index_days")
    varname_comparator_mdd_to_index_days <- paste0(this_comparator, "_mdd_to_index_days")
    
    varname_target_index     <- paste0(target_drug, "_Index")
    varname_comparator_index <- paste0(this_comparator, "_Index")
    
    this_ds <- open_dataset("Parquet_batched_OutputData/data_DTE_DiagnosisTimelineVars") %>%
      filter(batch_number == batch_num)
    
    diag_data <- this_ds %>% collect() %>%
      mutate(false_col = FALSE)
    
    this_ds <- open_dataset("Parquet_batched_OutputData/dte_cohort_wNontreat_data") %>%
      filter(batch_number == batch_num) %>%
      filter(!!sym(target_pop_colname) |
               !!sym(comparator_pop_colname))
    
    unmatched_data <-  this_ds %>% collect() %>%
      mutate(treatment = ifelse(!!sym(target_pop_colname) == TRUE, 1, 0)) %>%
      mutate(treatment_name = ifelse(!!sym(target_pop_colname) == TRUE, target_drug, this_comparator)) %>%
      mutate(age_at_index_years = ifelse(treatment,
                                         !!sym(varname_target_age_at_index_years),
                                         !!sym(varname_comparator_age_at_index_years))) %>%
      mutate(mdd_to_index_days = ifelse(treatment,
                                        !!sym(varname_target_mdd_to_index_days),
                                        !!sym(varname_comparator_mdd_to_index_days))) %>%
      mutate(index = as.Date(ifelse(treatment,
                                    !!sym(varname_target_index),
                                    !!sym(varname_comparator_index))),
             index_year = year(index)) %>%
      mutate(age_group_at_index_years = cut(age_at_index_years,
                                            breaks = c(0,18,25,45,65,85,150),
                                            include.lowest = T,
                                            right = F)) %>%
      mutate(age_group_at_index_years = plyr::revalue(age_group_at_index_years, c(
        "[0,18)"   = "Minor (<18)",
        "[18,25)"  = "Young Adult (18-24)",
        "[25,45)"  = "Adult (25-44)",
        "[45,65)"  = "Older Adult (45-64)",
        "[65,85)"  = "Senior (65-84)",
        "[85,150]" = "Elder (85+)")
      )) %>%
      mutate(mdd_to_index_years = time_length(interval(MDD_Index, index), "years")) %>%
      mutate(mdd_to_index_group = cut(mdd_to_index_years,
                                      breaks = c(0,1,2,5,10,15,20,200),
                                      include.lowest = T,
                                      right = F)) %>%
      mutate(mdd_to_index_group = plyr::revalue(mdd_to_index_group, c(
        "[0,1)"    = "0-1 years",
        "[1,2)"    = "1-2 years",
        "[2,5)"    = "2-5 years",
        "[5,10)"   = "5-10 years",
        "[10,15)"  = "10-15 years",
        "[15,20)"  = "15-20 years",
        "[20,200]" = ">20 years")
      )) %>%
      mutate(index_year_group = cut(index_year,
                                             breaks = c(0,2010,2015,2020,2025,3000),
                                             include.lowest = T,
                                             right = F)) %>%
      mutate(index_year_group = plyr::revalue(index_year_group, c(
        "[0,2010)"     = "Before 2010",
        "[2010,2015)"  = "2010-2015",
        "[2015,2020)"  = "2015-2020",
        "[2020,2025)"  = "2020-2025",
        "[2025,3000]"  = "2025 and beyond")
      )) %>%
      dplyr::select(-starts_with("batch_number.")) %>%
      left_join(diag_data, by = "PatientDurableKey")
    
    for (this_diag in eligibility_inclusion_diagnoses) {
      new_col    <- paste0(this_diag, "_Before_Drug_Index")
      target_col <- paste0(this_diag, "_Before_", target_drug, "_Index")
      other_col  <- paste0(this_diag, "_Before_", this_comparator, "_Index")
      
      unmatched_data <- unmatched_data %>%
        mutate(!!new_col := ifelse(!!sym(target_pop_colname),
                                   !!sym(target_col), !!sym(other_col)))
    }
    
    for (this_demo_drug in all_drugs) {
      if (this_demo_drug == target_drug) {
        target_col <- paste0(target_drug, "_Use")
      } else {
        target_col <- paste0(this_demo_drug, "_Overlaps_", target_drug, "_Index")
      }
      
      if (this_demo_drug == this_comparator) {
        other_col  <- paste0(this_demo_drug, "_Use")
      } else if(this_comparator == nontreatment_group){
        other_col <- "false_col"
      } else {
        other_col  <- paste0(this_demo_drug, "_Overlaps_", this_comparator, "_Index")
      } 
      
      new_col <- paste0(this_demo_drug, "_Overlaps_Drug_Index")
      
      unmatched_data <- unmatched_data %>%
        mutate(!!new_col := ifelse(!!sym(target_pop_colname),
                                   !!sym(target_col), !!sym(other_col)))
    }
    
    write_dataset(
      unmatched_data[seq_len(nrow(unmatched_data)), ] %>% # I have no idea why it has to be subsetted like this.
        mutate(batch_number = batch_num),
      path = paste0("Parquet_batched_OutputData/Unmatched_Dataset_", this_comparator),
      format = "parquet",
      partitioning = "batch_number"
    )
  }
}

# # -- Render nontreatment timelines report ------------------------------------------

source("get_Nontreatment_Summary.R")

ds_connect <- open_dataset(paste0("Parquet_batched_OutputData/Unmatched_Dataset_", nontreatment_group))

dte_cohort_wNontreat_data <- ds_connect %>%
  collect()

load(file = paste0("OutputData/", target_drug, "_tfe_dist.rds"))

n_sema <- open_dataset("Parquet_batched_prepped/dte_cohort_data") %>%
  filter(!!sym(paste0(target_drug, "_Use"))) %>%
  summarise(count = n())  %>%
  collect() %>%
  pull(count)

n_comp <- open_dataset("Parquet_batched_prepped/nonswitch_periods") %>%
  distinct(PatientDurableKey) %>%
  summarise(count = n())  %>%
  collect() %>%
  pull(count)

nontreat_result <- get_Nontreatment_Summary(
  n_sema                    = n_sema,
  n_comp                    = n_comp,
  dte_cohort_wNontreat_data = dte_cohort_wNontreat_data,
  target_drug               = target_drug,
  nontreatment_group        = nontreatment_group,
  tfe_dist                  = tfe_dist)

save(nontreat_result, file = paste0("OutputData/nontreatment_summary-", target_drug, ".rds"))

render(
  input       = "report_Nontreatment_Timelines.Rmd",
  output_file = paste0("Reports/report_Nontreatment_Timelines-", target_drug, ".html"),
  params      = list(
    summary_data       = nontreat_result,
    data               = dte_cohort_wNontreat_data
  ),
  envir = new.env()
)
rm(dte_cohort_wNontreat_data)
gc()


# -- Render eligibility criteria report -------------------------------------------

for (group in comparator_groups) {
  ds_connect_data <- open_dataset(paste0("Parquet_batched_OutputData/Unmatched_Dataset_", group))
  
  all.data <- ds_connect_data %>%
    collect()
  
  render(
    input       = "report_Eligibility_Criteria.Rmd",
    output_file = paste0("Reports/report_Eligibility_Criteria-", target_drug, "vs", group, ".html"),
    params      = list(
      eligibility_inclusion_diagnoses = eligibility_inclusion_diagnoses,
      comparator                      = group,
      var_name_to_pretty              = var_name_to_pretty,
      target_drug                     = target_drug,
      all.data                         = all.data
    ),
    envir = new.env()
  )
  gc()
}

# -- Render table 1 report -------------------------------------------

for (group in comparator_groups) {
  ds_connect_data <- open_dataset(paste0("Parquet_batched_OutputData/Unmatched_Dataset_", group))
  
  all.data <- ds_connect_data %>%
    collect()
  
  render(
    input       = "report_Table_1.Rmd",
    output_file = paste0("Reports/report_Table_1-", target_drug, "vs", group, ".html"),
    params      = list(
      comp_vars                       = comp_vars,
      comparator                      = group,
      var_name_to_pretty              = var_name_to_pretty,
      target_drug                     = target_drug,
      all.data                        = all.data
    ),
    envir = new.env()
  )
  gc()
}


# -- Compute outcomes --------------------------------------------------------------

matched_data_files <- setNames(
  paste0("Parquet_batched_OutputData/Unmatched_Dataset_", comparator_groups),
  comparator_groups
)

visits_file      <- "Parquet_batched_prepped/encounter_table"
med_changes_file <- "Parquet_batched_OutputData/antidepressant_antipsychotic_consecutive_period"
hc_med_file      <- "Parquet_batched_OutputData/hydrochlorothiazide_consecutive_instance"
psych_proc_file  <- "Parquet_batched_prepped/psych_proc"
diagnosis_file   <- "Parquet_batched_prepped/diagnosis_events_table"

outcomes_files <- list(
  psych   = paste0("Parquet_batched_OutputData/outcomes_psych-",         target_drug),
  visits  = paste0("Parquet_batched_OutputData/outcomes_visits-",        target_drug),
  med     = paste0("Parquet_batched_OutputData/outcomes_med_changes-",   target_drug),
  hc_med  = paste0("Parquet_batched_OutputData/outcomes_hc_med_changes-",target_drug),
  diag    = paste0("Parquet_batched_OutputData/outcomes_diagnosis-",     target_drug)
)

source("get_Outcomes_PsychProc.R")
source("get_Outcomes_Visits.R")
source("get_Outcomes_MedChanges.R")
source("get_Outcomes_HCMedChanges.R")
source("get_Outcomes_Diagnosis.R")

outcome_tasks <- list(
  list(fn = get_Outcomes_PsychProc,   src_file = psych_proc_file,   out_file = outcomes_files$psych),
  list(fn = get_Outcomes_Visits,      src_file = visits_file,       out_file = outcomes_files$visits),
  list(fn = get_Outcomes_MedChanges,  src_file = med_changes_file,  out_file = outcomes_files$med),
  list(fn = get_Outcomes_HCMedChanges,src_file = hc_med_file,       out_file = outcomes_files$hc_med),
  list(fn = get_Outcomes_Diagnosis   ,src_file = diagnosis_file,    out_file = outcomes_files$diag)
)

n_workers <- min(length(outcome_tasks), max(1L, detectCores(logical = TRUE) - 1L))
cl <- makeCluster(n_workers)
registerDoParallel(cl)

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  message("Running outcome computations in parallel (", n_workers, " workers)...")
  foreach(
    task      = outcome_tasks,
    .packages = c("dplyr", "tidyr", "lubridate", "readr", "arrow"),
    .export   = c("matched_data_files", "period_info", "target_drug", 
                  "comparator_groups", "batch_num",
                  "get_Outcomes_PsychProc",
                  "get_Outcomes_Visits",
                  "get_Outcomes_MedChanges",
                  "get_Outcomes_HCMedChanges",
                  "get_Outcomes_Diagnosis"
    )
  ) %dopar% {
    task$fn(task$src_file, matched_data_files, period_info,
            target_drug, comparator_groups, batch_num,
            task$out_file)
  }
}

stopCluster(cl)
message("All outcome computations complete.")
gc()

# -- Join outcome tables and pivot to long format ----------------------------------

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  outcomes_psych <- open_dataset(outcomes_files$psych) %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  outcomes_visits <- open_dataset(outcomes_files$visits) %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  outcomes_med_changes <- open_dataset(outcomes_files$med) %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  outcomes_hc_med_changes <- open_dataset(outcomes_files$hc_med) %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  outcomes_diagnosis <- open_dataset(outcomes_files$diag) %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  all_outcomes_wide <- outcomes_psych %>%
    left_join(outcomes_visits,
              by = c("PatientDurableKey", "study_cohort", "period")) %>%
    left_join(outcomes_med_changes,
              by = c("PatientDurableKey", "study_cohort", "period")) %>%
    left_join(outcomes_hc_med_changes,
              by = c("PatientDurableKey", "study_cohort", "period")) %>%
    left_join(outcomes_diagnosis,
              by = c("PatientDurableKey", "study_cohort", "period")) %>%
    left_join(period_info %>% dplyr::select(period, period_alias), by = "period") %>%
    relocate(period_alias, .after = "study_cohort") %>%
    dplyr::select(-period, -starts_with("batch_number"))
  
  all_outcomes <- all_outcomes_wide %>%
    pivot_longer(4:ncol(.), names_to = "var_name", values_to = "value") %>%
    mutate(
      period_alias = factor(period_alias, levels = period_info$period_alias),
      study_cohort = factor(study_cohort, levels = paste0(target_drug, " vs ", comparator_groups)),
      var_name     = factor(var_name)
    )
  
  write_dataset(
    all_outcomes %>%
      mutate(batch_number = batch_num),
    path = paste0("Parquet_batched_OutputData/all_outcomes-", target_drug),
    format = "parquet",
    partitioning = c("study_cohort", "var_name", "period_alias", "batch_number")
  )
  gc()
}

rm(all_outcomes, 
   all_outcomes_wide, 
   outcomes_psych, 
   outcomes_visits, 
   outcomes_med_changes, 
   outcomes_hc_med_changes)
gc()

# -- Negative Binomial Regression analyses -----------------------------------------

source("analysis_Negative_Binomial_Regression.R")


matched_data_files <- setNames(
  paste0("Parquet_batched_OutputData/Unmatched_Dataset_", comparator_groups),
  comparator_groups
)

all_outcomes <- c("n_psych_days", "n_med_changes", "n_Intentional_Self_Harm_diagnoses",
                  "n_Suicidal_Ideation_diagnoses", "n_Suicide_Attempt_diagnoses", 
                  "n_External_Causes_of_Morbidity_diagnoses")

covariates <- c("Race_Ethnicity_white", "Sex_male", "age_at_index_years")

nb_analyses <- list()
i <- 1
for(period in period_info$period_alias){
  for(dep_var in all_outcomes){
      nb_analyses[[i]] <- list(dep_var = dep_var,
                               period = period,
                               covariates = covariates
                               )
      i <- i+1
  }
}

nb_result_files <- character(0)

for (group in comparator_groups) {
  for (analysis in nb_analyses) {
    
    result_suffix <- paste0(target_drug, "Vs", group,
                            "-", analysis$dep_var, 
                            "-period", analysis$period)
    
    result_file <- paste0(
      "OutputData/nb_result-", result_suffix, 
      ".rds"
    )
    
    period_name <- period_info$period[period_info$period_alias == analysis$period]
    
    message("Fitting NB model: ", target_drug, " vs ", group,
            " | ", analysis$dep_var, " | period ", period_name)
    
    study_cohort_label <- paste0(target_drug, " vs ", group)
    
    ds_connect <- open_dataset(paste0("Parquet_batched_OutputData/all_outcomes-", target_drug))
    this_outcome <- ds_connect %>%
      filter(study_cohort == study_cohort_label,
             period_alias == analysis$period,
             var_name == analysis$dep_var) %>%
      dplyr::select(c("PatientDurableKey", "var_name", "value")) %>% 
      collect() %>%
      pivot_wider(names_from = "var_name", values_from = "value")
    
    ds_connect <- open_dataset(matched_data_files[[group]])
    matched_data <- ds_connect %>% 
      dplyr::select(c("PatientDurableKey", "treatment", "treatment_name", analysis$covariates)) %>% 
      collect()
    
    analysis_data <- matched_data %>%
      left_join(this_outcome, by = c("PatientDurableKey"))
    
    rm(this_outcome)
    rm(matched_data)
    gc()
    
    analysis_Negative_Binomial_Regression(
      analysis_data     = analysis_data,
      comparator_group  = group,
      target_drug       = target_drug,
      period_name       = period_name,
      dep_var           = analysis$dep_var,
      covariates        = analysis$covariates,
      output_file       = result_file
    )
    
    nb_result_files <- c(nb_result_files, result_file)
    
    render(
      input       = "report_Negative_Binomial_Regression.Rmd",
      output_file = paste0("Reports/report_NB-", result_suffix, ".html"),
      params = list(
        analysis_data    = analysis_data,
        result_file      = result_file,
        target_drug      = target_drug,
        comparator_group = group,
        dep_var          = analysis$dep_var
      ),
      envir = new.env()
    )
    
    rm(analysis_data)
    gc()
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
gc()

# -- Negative Binomial Regression analyses with CBPS -----------------------------------------

source("analysis_Propensity_Scoring_Variable_Selection.R")

matched_data_files <- setNames(
  paste0("Parquet_batched_OutputData/Unmatched_Dataset_", comparator_groups),
  comparator_groups
)

all_outcomes <- c(#"n_psych_days", "n_med_changes", 
                  "n_Intentional_Self_Harm_diagnoses",
                  "n_Suicidal_Ideation_diagnoses", "n_Suicide_Attempt_diagnoses", 
                  "n_External_Causes_of_Morbidity_diagnoses")

nb_analyses <- list()
i <- 1
for(period in period_info$period_alias){
  for(dep_var in all_outcomes){
    nb_analyses[[i]] <- list(dep_var = dep_var,
                             period = period
    )
    i <- i+1
  }
}

nb_result_files <- character(0)

total_models <- length(comparator_groups) * length(nb_analyses)
i <- 0

for (group in comparator_groups) {
  for (analysis in nb_analyses) {

    i <- i + 1
    percent_complete <- percent(i/total_models, accuracy = 0.01)
    
    result_suffix <- paste0(target_drug, "Vs", group,
                            "-", analysis$dep_var, 
                            "-period", analysis$period)
    
    dataset_file <- paste0(
      "OutputData/cbps_nb_dataset-", result_suffix
    )
    
    vs_result_file <- paste0(
      "OutputData/cbps_nb_vs_result-", result_suffix, 
      ".rds"
    )
    
    result_file <- paste0(
      "OutputData/cbps_nb_result-", result_suffix, 
      ".rds"
    )
    
    if(file.exists(result_file)){next}
    
    period_name <- period_info$period[period_info$period_alias == analysis$period]
    
    message("Fitting NB model: ", target_drug, " vs ", group,
            " | ", analysis$dep_var, " | period ", period_name)
    
    study_cohort_label <- paste0(target_drug, " vs ", group)
    
    ds_connect <- open_dataset(paste0("Parquet_batched_OutputData/all_outcomes-", target_drug))
    this_outcome <- ds_connect %>%
      filter(study_cohort == study_cohort_label,
             period_alias == analysis$period,
             var_name == analysis$dep_var) %>%
      dplyr::select(c("PatientDurableKey", "var_name", "value")) %>% 
      collect() %>%
      pivot_wider(names_from = "var_name", values_from = "value")
    
    ds_connect <- open_dataset(matched_data_files[[group]])
    matched_data <- ds_connect %>% 
      dplyr::select(c("PatientDurableKey", "treatment", "treatment_name", ps_covariates$var, "batch_number")) %>% 
      collect()
    
    analysis_data <- matched_data %>%
      left_join(this_outcome, by = c("PatientDurableKey"))
    
    rm(this_outcome)
    rm(matched_data)
    gc()
    
    vs_res <- analysis_Propensity_Scoring_Variable_Selection(
      comparator_group = group,
      target_drug = target_drug,
      cohort_table = analysis_data,
      covariates_table = ps_covariates
    )
    
    analysis_data <- analysis_data %>%
      mutate(across(all_of(vs_res$logical_vars), ~ as.numeric(.)))
    
    write_dataset(
      analysis_data,
      path = dataset_file,
      format = "parquet",
      partitioning = "batch_number"
    )
    
    matchingFormula <- as.formula(paste0("treatment ~ ",
                                         paste(vs_res$matchingVars.final$var, collapse = " + ")))
    
    res <- bigcbps(matchingFormula,
                   outcome = analysis$dep_var,
                   data = dataset_file,
                   family = "nb")
    
    save(vs_res, file = vs_result_file)
    save(res, file = result_file)
    
    rm(analysis_data)
    gc()
    
    message(i, " of ", total_models, " complete (", percent_complete, ")")
  }
}

# render(
#   input       = "report_NB_Summary.Rmd",
#   output_file = paste0("Reports/report_NB_Summary-", target_drug, ".html"),
#   params = list(
#     result_files = nb_result_files,
#     target_drug  = target_drug
#   ),
#   envir = new.env()
# )
# gc()

# -- PWP Gap Time Cox Model analyses -----------------------------------------------

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
gc()
