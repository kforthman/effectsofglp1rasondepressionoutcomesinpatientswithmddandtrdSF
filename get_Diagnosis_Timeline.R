all_drugs <- c(target_drug, comparator_drugs)
all_diagnoses <- c("T1DM", "T2DM", "Hypertension", "Heart_Disease", "Hyperlipidemia", "Obesity", "Hypercholesterolemia", "Chronic_Kidney_Disease", "A1C_over_8p5", "Pancreatitis", "Stroke", "Thyroid_Cancer", "Gastroparesis")
index_dataset <- nontreat_result$dte_cohort_data2

# %%
define_before_diagnosis_ids <- function(index_drug_record, diagnosis_timeline){
  this_group <- index_drug_record %>%
    pull(PatientDurableKey)
  
  index_before_timeline <- diagnosis_timeline %>% filter(PatientDurableKey %in% this_group) %>%
    left_join(
      index_drug_record,
      by = "PatientDurableKey"
    ) %>%
    mutate(time_from_index_months = time_length(interval(index_drug_start_datetime,
                                                         FirstDiagnosisDate),
                                                "months")) %>%
    filter(time_from_index_months <= 0) %>%
    arrange(PatientDurableKey, FirstDiagnosisDate)
  
  index_before_id <- index_before_timeline %>% dplyr::select(PatientDurableKey) %>% group_by(PatientDurableKey) %>% slice(1) %>% ungroup
  
  return(index_before_id)
}

# %%
all.data <- dte_cohort_data %>% dplyr::select(PatientDurableKey)
for(i in 1:length(all_diagnoses)){
  this_diagnosis <- all_diagnoses[i]
  this_diagnosis_data <- diag_table %>% filter(Diagnosis == this_diagnosis)
  for(j in 1:length(all_drugs)){
    this_drug <- all_drugs[j]
    this_index_drug_data <- index_dataset %>% 
      dplyr::select(PatientDurableKey, !!sym(paste0(this_drug, "_Index"))) %>%
      filter(!is.na(!!sym(paste0(this_drug, "_Index")))) %>%
      rename(index_drug_start_datetime = paste0(this_drug, "_Index"))
    this_before <- define_before_diagnosis_ids(this_index_drug_data, this_diagnosis_data) %>% pull(PatientDurableKey)
    all.data <- all.data %>% mutate(!!sym(paste0(this_diagnosis, "_Before_", this_drug, "_Index")) := PatientDurableKey %in% this_before)
  }
}

# %%
this.data <- all.data
save(this.data, file = "OutputData/data_DTE_DiagnosisTimelineVars.rds")

