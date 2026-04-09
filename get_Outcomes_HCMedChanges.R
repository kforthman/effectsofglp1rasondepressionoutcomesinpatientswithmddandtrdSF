# -*- coding: utf-8 -*-
# get_Outcomes_HCMedChanges(): Computes hydrochlorothiazide medication change
# outcomes (active comparator sensitivity analysis) for all comparator groups
# and time periods.
#
# Arguments:
#   hc_med_file        — Path to the hydrochlorothiazide consecutive instance
#                        .rds file. Must restore variable
#                        `consecutive_instance_tab` with columns: person_id,
#                        first_record.
#   matched_data_files — Named character vector mapping each comparator group
#                        name to its PS_Matched_Dataset .rds file path.
#   period_info        — Data frame with columns: period (label), bgn_win,
#                        end_win (days from index).
#   target_drug        — Name of the target treatment (e.g. "Semaglutide").
#   nontreatment_group — Name of the nontreatment comparator (e.g. "Nontreatment").
#   comparator_groups  — Character vector of comparator group names.
#   output_file        — Path to save the result .rds file.
#
# Returns (invisibly):
#   Wide data frame: one row per (person_id × study_cohort × period) with
#   column n_hc_med_changes. Also saved to output_file as `outcomes`.

get_Outcomes_HCMedChanges <- function(hc_med_file,
                                      matched_data_files,
                                      period_info,
                                      target_drug,
                                      nontreatment_group,
                                      comparator_groups,
                                      output_file) {

  load(hc_med_file)   # restores consecutive_instance_tab

  results <- vector("list", length(comparator_groups) * nrow(period_info))
  k <- 0L

  for (group in comparator_groups) {
    load(matched_data_files[[group]])   # restores matched.data

    index_ext <- ifelse(group == nontreatment_group, "_index", "_first_drug_record")
    matched.data <- matched.data %>%
      mutate(first_drug_record = if_else(
        treatment_name == target_drug,
        !!sym(paste0(target_drug, "_first_drug_record")),
        !!sym(paste0(group, index_ext))
      ))

    study_cohort_label <- paste0(target_drug, " vs ", group)
    treatment_levels   <- c(target_drug, group)

    matched_slim <- matched.data %>%
      dplyr::select(person_id, treatment, treatment_name, first_drug_record)

    these_ids <- matched.data %>%
      dplyr::select(person_id) %>%
      mutate(study_cohort = study_cohort_label)

    # Pre-join HC med records to matched cohort (done once per group)
    hc_drugs_for_group <- consecutive_instance_tab %>%
      filter(person_id %in% matched.data$person_id) %>%
      left_join(matched_slim, by = "person_id") %>%
      mutate(
        time_from_index_days = as.numeric(difftime(first_record, first_drug_record, units = "days")),
        treatment_name       = factor(treatment_name, levels = treatment_levels),
        study_cohort         = study_cohort_label
      )

    for (j in seq_len(nrow(period_info))) {
      bgn <- period_info$bgn_win[j]
      end <- period_info$end_win[j]
      prd <- period_info$period[j]

      hc_drugs_w <- hc_drugs_for_group %>%
        filter(time_from_index_days >= bgn & time_from_index_days < end)

      hc_med_changes <- hc_drugs_w %>%
        group_by(person_id, study_cohort) %>%
        summarize(n_hc_med_changes = n(), .groups = "drop")

      k <- k + 1L
      results[[k]] <- these_ids %>%
        mutate(period = prd) %>%
        left_join(hc_med_changes, by = c("person_id", "study_cohort")) %>%
        mutate(n_hc_med_changes = replace_na(n_hc_med_changes, 0L))
    }
  }

  outcomes <- bind_rows(results)
  save(outcomes, file = output_file)
  invisible(outcomes)
}
