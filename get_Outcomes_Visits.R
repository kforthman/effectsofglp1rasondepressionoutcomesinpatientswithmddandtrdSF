# -*- coding: utf-8 -*-
# get_Outcomes_Visits(): Computes visit-based outcomes (total visits and
# inpatient days) for all comparator groups and time periods.
#
# Arguments:
#   visits_file        — Path to the dte_cohort_visits .rds file. Must restore
#                        variable `dte_cohort_visits` with columns: person_id,
#                        visit_occurrence_id, start_datetime, end_datetime,
#                        standard_concept_name.
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
#   columns n_visits and n_visit_days. Also saved to output_file as `outcomes`.

get_Outcomes_Visits <- function(visits_file,
                                matched_data_files,
                                period_info,
                                target_drug,
                                nontreatment_group,
                                comparator_groups,
                                output_file) {

  load(visits_file)   # restores dte_cohort_visits

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

    # Pre-join visits to matched cohort (done once per group)
    visits_for_group <- dte_cohort_visits %>%
      filter(person_id %in% matched.data$person_id) %>%
      left_join(matched_slim, by = "person_id") %>%
      mutate(
        time_from_index_days = as.numeric(difftime(start_datetime, first_drug_record, units = "days")),
        treatment_name       = factor(treatment_name, levels = treatment_levels),
        n_days               = as.numeric(difftime(end_datetime, start_datetime, units = "days")) + 1,
        study_cohort         = study_cohort_label
      )

    for (j in seq_len(nrow(period_info))) {
      bgn <- period_info$bgn_win[j]
      end <- period_info$end_win[j]
      prd <- period_info$period[j]

      visits_w <- visits_for_group %>%
        filter(time_from_index_days >= bgn & time_from_index_days < end)

      total_visits <- visits_w %>%
        group_by(person_id, study_cohort) %>%
        summarize(n_visits = n(), .groups = "drop")

      visit_days <- visits_w %>%
        uncount(n_days) %>%
        group_by(person_id, visit_occurrence_id, study_cohort) %>%
        mutate(visit_date    = start_datetime + days(row_number() - 1)) %>%
        mutate(time_to_visit = interval(first_drug_record, visit_date) / days(1)) %>%
        filter(time_to_visit >= bgn & time_to_visit <= end) %>%
        group_by(person_id, visit_date, study_cohort) %>%
        slice_head(n = 1) %>%
        group_by(person_id, study_cohort) %>%
        summarize(n_visit_days = n(), .groups = "drop")

      k <- k + 1L
      results[[k]] <- these_ids %>%
        mutate(period = prd) %>%
        left_join(total_visits, by = c("person_id", "study_cohort")) %>%
        left_join(visit_days,   by = c("person_id", "study_cohort")) %>%
        mutate(
          n_visits     = replace_na(n_visits,     0L),
          n_visit_days = replace_na(n_visit_days, 0L)
        )
    }
  }

  outcomes <- bind_rows(results)
  save(outcomes, file = output_file)
  invisible(outcomes)
}
