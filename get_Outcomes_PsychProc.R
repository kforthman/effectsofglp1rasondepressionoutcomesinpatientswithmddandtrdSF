# -*- coding: utf-8 -*-
# get_Outcomes_PsychProc(): Computes psychiatric procedure outcomes for all
# comparator groups and time periods from the psych proc (Outcomes) table.
#
# Arguments:
#   psych_proc_file    — Path to the psych proc CSV (Outcomes_Table).
#                        Must contain: person_id, start_datetime, level,
#                        source_concept_code.
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
#   columns for all psych proc outcomes. Also saved to output_file as `outcomes`.

get_Outcomes_PsychProc <- function(psych_proc_file,
                                   matched_data_files,
                                   period_info,
                                   target_drug,
                                   nontreatment_group,
                                   comparator_groups,
                                   output_file) {

  psych_proc <- read_csv(psych_proc_file,
                         na       = c("", "NA", "NULL", "null"),
                         col_types = cols(
                           PatientDurableKey = col_character(),
                           OutcomeDate       = col_date(format = "%Y-%m-%d"),
                           OutcomeName       = col_character(),
                           CPTCode           = col_character()
                         ))

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

    # Pre-join psych data to matched cohort (done once per group)
    psych_for_group <- psych_proc %>%
      filter(person_id %in% matched.data$person_id) %>%
      left_join(matched_slim, by = "person_id") %>%
      mutate(
        time_from_index_days = as.numeric(difftime(start_datetime, first_drug_record, units = "days")),
        treatment_name       = factor(treatment_name, levels = treatment_levels),
        study_cohort         = study_cohort_label
      )

    for (j in seq_len(nrow(period_info))) {
      bgn <- period_info$bgn_win[j]
      end <- period_info$end_win[j]
      prd <- period_info$period[j]

      psych_w <- psych_for_group %>%
        filter(time_from_index_days >= bgn & time_from_index_days < end)

      # Pass 1 – aggregate counts and acuity
      psych_agg <- psych_w %>%
        group_by(person_id, study_cohort) %>%
        summarize(
          n_psych_visits   = n(),
          n_psych_days     = n_distinct(start_datetime),
          cum_acuity_psych = sum(level),
          max_acuity_psych = max(level),
          n_level1_psych   = sum(level == 1L),
          n_level2_psych   = sum(level == 2L),
          n_level3_psych   = sum(level == 3L),
          n_level4_psych   = sum(level == 4L),
          n_level5_psych   = sum(level == 5L),
          .groups = "drop"
        )

      # Pass 2 – time to 1st / 2nd / 3rd unique psych visit day
      psych_nth <- psych_w %>%
        distinct(person_id, study_cohort, time_from_index_days) %>%
        arrange(person_id, study_cohort, time_from_index_days) %>%
        group_by(person_id, study_cohort) %>%
        summarize(
          time_to_first_psych  = time_from_index_days[1L],
          time_to_second_psych = time_from_index_days[2L],
          time_to_third_psych  = time_from_index_days[3L],
          .groups = "drop"
        )

      # Pass 3 – time to 1st / 2nd / 3rd unique level-2 psych visit day
      psych_level2_nth <- psych_w %>%
        filter(level == 2L) %>%
        distinct(person_id, study_cohort, time_from_index_days) %>%
        arrange(person_id, study_cohort, time_from_index_days) %>%
        group_by(person_id, study_cohort) %>%
        summarize(
          time_to_1st_level2_psych = time_from_index_days[1L],
          time_to_2nd_level2_psych = time_from_index_days[2L],
          time_to_3rd_level2_psych = time_from_index_days[3L],
          .groups = "drop"
        )

      k <- k + 1L
      results[[k]] <- these_ids %>%
        mutate(period = prd) %>%
        left_join(psych_agg,        by = c("person_id", "study_cohort")) %>%
        left_join(psych_nth,        by = c("person_id", "study_cohort")) %>%
        left_join(psych_level2_nth, by = c("person_id", "study_cohort")) %>%
        mutate(
          n_psych_visits = replace_na(n_psych_visits, 0L),
          n_psych_days   = replace_na(n_psych_days,   0L)
        )
    }
  }

  outcomes <- bind_rows(results)
  save(outcomes, file = output_file)
  invisible(outcomes)
}
