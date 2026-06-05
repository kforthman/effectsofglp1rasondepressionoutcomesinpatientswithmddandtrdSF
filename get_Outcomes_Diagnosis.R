# -*- coding: utf-8 -*-
# get_Outcomes_Visits(): Computes visit-based outcomes (total visits and
# inpatient days) for all comparator groups and time periods.
#
# Arguments:
#   diagnosis_file     - Path to the file.
#   matched_data_files - Named character vector mapping each comparator group
#                        name to its PS_Matched_Dataset .rds file path.
#   period_info        - Data frame with columns: period (label), bgn_win,
#                        end_win (days from index).
#   target_drug        - Name of the target treatment (e.g. "Semaglutide").
#   comparator_groups  - Character vector of comparator group names.
#   output_file        - Path to save the result .rds file.
#
# Returns (invisibly):
#   Wide data frame: one row per (PatientDurableKey x study_cohort x period) with
#   columns n_visits and n_visit_days. Also saved to output_file as `outcomes`.

get_Outcomes_Diagnosis <- function(diagnosis_file,
                                matched_data_files,
                                period_info,
                                target_drug,
                                comparator_groups,
                                batch_num,
                                output_file) {
  
  diagnosis_table <- open_dataset(diagnosis_file) %>%
    filter(batch_number == batch_num) %>%
    collect()
  
  results <- vector("list", length(comparator_groups) * nrow(period_info))
  k <- 0L
  
  for (group in comparator_groups) {
    matched.data <- open_dataset(matched_data_files[[group]]) %>%
      filter(batch_number == batch_num) %>%
      collect()
    
    matched.data <- matched.data %>%
      mutate(index_date = if_else(
        treatment_name == target_drug,
        !!sym(paste0(target_drug, "_Index")),
        !!sym(paste0(group, "_Index"))
      ))
    
    study_cohort_label <- paste0(target_drug, " vs ", group)
    treatment_levels   <- c(target_drug, group)
    
    matched_slim <- matched.data %>%
      dplyr::select(PatientDurableKey, treatment, treatment_name, index_date)
    
    these_ids <- matched.data %>%
      dplyr::select(PatientDurableKey) %>%
      mutate(study_cohort = study_cohort_label)
    
    # Pre-join visits to matched cohort (done once per group)
    diagnosis_for_group <- diagnosis_table %>%
      filter(PatientDurableKey %in% matched.data$PatientDurableKey) %>%
      left_join(matched_slim, by = "PatientDurableKey") %>%
      mutate(
        time_from_index_days = as.numeric(difftime(DiagnosisDate, index_date, units = "days")),
        treatment_name       = factor(treatment_name, levels = treatment_levels),
        study_cohort         = study_cohort_label
      )
    
    for (j in seq_len(nrow(period_info))) {
      bgn <- period_info$bgn_win[j]
      end <- period_info$end_win[j]
      prd <- period_info$period[j]
      
      diagnosis_w <- diagnosis_for_group %>%
        filter(time_from_index_days >= bgn & time_from_index_days < end)
      
      time_to_diagnosis <- diagnosis_w %>%
        distinct(PatientDurableKey, Diagnosis, study_cohort, time_from_index_days) %>%
        arrange(PatientDurableKey, Diagnosis, study_cohort, time_from_index_days) %>%
        group_by(PatientDurableKey, Diagnosis, study_cohort) %>%
        summarize(
          time_to_first_diagnosis  = time_from_index_days[1L],
          .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
          names_from = Diagnosis,
          values_from = time_to_first_diagnosis,
          names_glue = "time_to_first_{Diagnosis}_diagnosis"
        )
      
      total_diagnoses <- diagnosis_w %>%
        group_by(PatientDurableKey, Diagnosis, study_cohort) %>%
        summarize(n_diagnoses = n(), 
                  .groups = "drop") %>%
        tidyr::pivot_wider(
          names_from = Diagnosis,
          values_from = n_diagnoses,
          names_glue = "n_{Diagnosis}_diagnoses"
        )
      
      k <- k + 1L
      results[[k]] <- these_ids %>%
        mutate(period = prd) %>%
        left_join(time_to_diagnosis, by = c("PatientDurableKey", "study_cohort")) %>%
        left_join(total_diagnoses,   by = c("PatientDurableKey", "study_cohort")) %>%
        mutate(across(matches("^n_.*_diagnoses$"), ~ coalesce(.x, 0)))
    }
  }
  
  outcomes <- bind_rows(results)
  # invisible(outcomes)
  
  write_dataset(
    outcomes %>%
      mutate(batch_number = batch_num),
    path = output_file,
    format = "parquet",
    partitioning = "batch_number"
  )
}
