# -*- coding: utf-8 -*-
# get_Outcomes_Visits(): Computes visit-based outcomes (total visits and
# inpatient days) for all comparator groups and time periods.
#
# Arguments:
#   visits_file        — Path to the encounter_table.rds file. Must restore
#                        variable `encounter_table` with columns: PatientDurableKey,
#                        EncounterKey, StartVisit, EndVisit,
#                        standard_concept_name.
#   matched_data_files — Named character vector mapping each comparator group
#                        name to its PS_Matched_Dataset .rds file path.
#   period_info        — Data frame with columns: period (label), bgn_win,
#                        end_win (days from index).
#   target_drug        — Name of the target treatment (e.g. "Semaglutide").
#   comparator_groups  — Character vector of comparator group names.
#   output_file        — Path to save the result .rds file.
#
# Returns (invisibly):
#   Wide data frame: one row per (PatientDurableKey × study_cohort × period) with
#   columns n_visits and n_visit_days. Also saved to output_file as `outcomes`.

get_Outcomes_Visits <- function(visits_file,
                                matched_data_files,
                                period_info,
                                target_drug,
                                comparator_groups,
                                output_file) {

  load(visits_file)   # restores encounter_table

  results <- vector("list", length(comparator_groups) * nrow(period_info))
  k <- 0L

  for (group in comparator_groups) {
    load(matched_data_files[[group]])   # restores matched.data

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
    visits_for_group <- encounter_table %>%
      filter(PatientDurableKey %in% matched.data$PatientDurableKey) %>%
      left_join(matched_slim, by = "PatientDurableKey") %>%
      mutate(
        time_from_index_days = as.numeric(difftime(StartVisit, index_date, units = "days")),
        treatment_name       = factor(treatment_name, levels = treatment_levels),
        n_days               = as.numeric(difftime(EndVisit, StartVisit, units = "days")) + 1,
        study_cohort         = study_cohort_label
      )

    for (j in seq_len(nrow(period_info))) {
      bgn <- period_info$bgn_win[j]
      end <- period_info$end_win[j]
      prd <- period_info$period[j]

      visits_w <- visits_for_group %>%
        filter(time_from_index_days >= bgn & time_from_index_days < end)
      
      time_to_visit <- visits_w %>%
        distinct(PatientDurableKey, study_cohort, time_from_index_days) %>%
        arrange(PatientDurableKey, study_cohort, time_from_index_days) %>%
        group_by(PatientDurableKey, study_cohort) %>%
        summarize(
          time_to_first_visit  = time_from_index_days[1L],
          .groups = "drop"
        )
      
      time_to_visit_class_agg <- visits_w %>%
        distinct(PatientDurableKey, study_cohort, PatientClass, time_from_index_days) %>%
        arrange(PatientDurableKey, study_cohort, PatientClass, time_from_index_days) %>%
        group_by(PatientDurableKey, study_cohort, PatientClass) %>%
        summarize(
          time_to_first_visit = first(time_from_index_days),
          .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
          names_from = PatientClass,
          values_from = time_to_first_visit,
          names_glue = "time_to_first_{tolower(PatientClass)}_visit"
        )

      total_visits <- visits_w %>%
        group_by(PatientDurableKey, study_cohort) %>%
        summarize(n_visits             = n(), 
                  n_emergency_visits   = sum(PatientClass == "Emergency"),
                  n_inpatient_visits   = sum(PatientClass == "Inpatient"),
                  n_observation_visits = sum(PatientClass == "Observation"),
                  n_outpatient_visits  = sum(PatientClass == "Outpatient"),
                  .groups = "drop")
      
      visits_by_day <- visits_w %>%
        uncount(n_days) %>%
        group_by(PatientDurableKey, EncounterKey, study_cohort) %>%
        mutate(visit_date    = StartVisit + days(row_number() - 1)) %>%
        mutate(time_to_visit = interval(index_date, visit_date) / days(1)) %>%
        filter(time_to_visit >= bgn & time_to_visit <= end) %>%
        group_by(PatientDurableKey, visit_date, study_cohort) %>%
        slice_head(n = 1) %>%
        ungroup()
      
      visits_by_day_agg <- visits_by_day %>%
        group_by(PatientDurableKey, study_cohort) %>%
        summarize(
          n_visit_days             = n(),
          n_emergency_visit_days   = sum(PatientClass == "Emergency"),
          n_inpatient_visit_days   = sum(PatientClass == "Inpatient"),
          n_observation_visit_days = sum(PatientClass == "Observation"),
          .groups = "drop"
        )

      k <- k + 1L
      results[[k]] <- these_ids %>%
        mutate(period = prd) %>%
        left_join(time_to_visit,           by = c("PatientDurableKey", "study_cohort")) %>%
        left_join(time_to_visit_class_agg, by = c("PatientDurableKey", "study_cohort")) %>%
        left_join(total_visits,            by = c("PatientDurableKey", "study_cohort")) %>%
        left_join(visits_by_day_agg,       by = c("PatientDurableKey", "study_cohort")) %>%
        mutate(
          n_visits                 = replace_na(n_visits,     0L),
          n_emergency_visits       = replace_na(n_emergency_visits, 0L),
          n_inpatient_visits       = replace_na(n_inpatient_visits, 0L),
          n_observation_visits     = replace_na(n_observation_visits, 0L),
          n_outpatient_visits      = replace_na(n_outpatient_visits, 0L),
          n_visit_days             = replace_na(n_visit_days, 0L),
          n_emergency_visit_days   = replace_na(n_emergency_visit_days, 0L),
          n_inpatient_visit_days   = replace_na(n_inpatient_visit_days, 0L),
          n_observation_visit_days = replace_na(n_observation_visit_days, 0L)
        )
    }
  }

  outcomes <- bind_rows(results)
  save(outcomes, file = output_file)
  invisible(outcomes)
}
