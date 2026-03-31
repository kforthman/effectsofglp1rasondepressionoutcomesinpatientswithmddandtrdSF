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

# get_Diagnosis_Timeline(): For each combination of diagnosis and drug index date,
# flags whether each participant had that diagnosis before their index date.
#
# Arguments:
#   target_drug      — Name of the target drug (e.g. "Semaglutide")
#   comparator_groups — Character vector of comparator drug names
#   all_diagnoses    — Character vector of diagnosis names to evaluate
#   index_dataset    — Cohort data frame with PatientDurableKey and {drug}_Index columns
#   diag_table       — Diagnosis data frame with PatientDurableKey, Diagnosis, FirstDiagnosisDate
#   output_filename  — Output path for the diagnosis timeline variables (.rds)
#
# Saves (written to disk, no return value):
#   {output_filename} — this.data (PatientDurableKey + one logical column per diagnosis/drug combo)

get_Diagnosis_Timeline <- function(
    target_drug,
    comparator_groups,
    all_diagnoses,
    index_dataset,
    diag_table,
    output_filename
) {
  
  all_drugs <- c(target_drug, comparator_groups)
  
  all.data <- index_dataset %>% dplyr::select(PatientDurableKey) %>% distinct()
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
  
  this.data <- all.data
  save(this.data, file = output_filename)
}
