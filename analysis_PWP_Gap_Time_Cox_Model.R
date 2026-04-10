# -*- coding: utf-8 -*-
# analysis_PWP_Gap_Time_Cox_Model(): Fits a Prentice-Williams-Peterson (PWP)
# gap time Cox model for recurrent events.
#
# Arguments:
#   matched_data_file — Path to PS_Matched_Dataset-{group}.rds. Must restore
#                       `matched.data` with columns PatientDurableKey,
#                       treatment, treatment_name, {target_drug}_Index,
#                       {comparator_group}_Index, and covariate columns.
#   event_data_file   — Path to event data .rds file. The file may restore any
#                       variable name; the first restored object is used.
#                       Must contain PatientDurableKey and event_date_col.
#   event_date_col    — Column name for the event date in the event data
#                       (e.g. "OutcomeDate" for psych visits, "first_record"
#                       for medication change periods).
#   dedup_by_day      — Logical. If TRUE, deduplicate events to at most one
#                       per patient per day (use for visit-based outcomes).
#   comparator_group  — Comparator group name (e.g. "Insulins").
#   target_drug       — Target drug name (e.g. "Semaglutide").
#   period_name       — Full period label (e.g. "15 days-12 months after index").
#   bgn_win           — Period start in days from index (inclusive).
#   end_win           — Period end in days from index (exclusive); also used
#                       as the administrative censoring time.
#   dep_var           — Short outcome label used for event naming and file
#                       naming (e.g. "psych_visits" or "med_changes").
#   covariates        — Character vector of covariate column names.
#   output_file       — Path to save the result .rds file.
#
# Returns (invisibly):
#   Named list with model, counting-process dataset, and metadata.
#   Also saved to output_file as `pwp_result`.

analysis_PWP_Gap_Time_Cox_Model <- function(matched_data_file,
                                             event_data_file,
                                             event_date_col,
                                             dedup_by_day,
                                             comparator_group,
                                             target_drug,
                                             period_name,
                                             bgn_win,
                                             end_win,
                                             dep_var,
                                             covariates,
                                             output_file) {

  load(matched_data_file)              # restores matched.data
  loaded_var <- load(event_data_file)  # restores named variable
  event_raw  <- get(loaded_var[1])

  study_cohort_label <- paste0(target_drug, " vs ", comparator_group)

  matched.data <- matched.data %>%
    mutate(
      index_date = if_else(
        treatment_name == target_drug,
        !!sym(paste0(target_drug,      "_Index")),
        !!sym(paste0(comparator_group, "_Index"))
      ),
      study_cohort = study_cohort_label
    )

  # Compute time from index for each event record
  event_data <- event_raw %>%
    filter(PatientDurableKey %in% matched.data$PatientDurableKey) %>%
    left_join(
      matched.data %>%
        dplyr::select(PatientDurableKey, treatment, treatment_name, index_date),
      by = "PatientDurableKey"
    ) %>%
    mutate(
      time_from_index_days = as.numeric(
        difftime(!!sym(event_date_col), index_date, units = "days")
      ),
      treatment_name = factor(treatment_name, levels = c(target_drug, comparator_group)),
      study_cohort   = study_cohort_label
    ) %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win)
  
  if (dedup_by_day) {
    event_data <- event_data %>%
      arrange(PatientDurableKey, time_from_index_days) %>%
      group_by(PatientDurableKey, time_from_index_days) %>%
      slice_head(n = 1) %>%
      ungroup()
  }
  
  event_data <- event_data %>%
    dplyr::select(PatientDurableKey, time_from_index_days) %>%
    mutate(event = dep_var) %>%
    rename(event_time = time_from_index_days)

  # Build counting-process dataset
  these_ids <- matched.data$PatientDurableKey
  
  event_end <- data.frame(
    PatientDurableKey = these_ids,
    event             = "end_followup",
    event_time        = end_win,
    stringsAsFactors  = FALSE
  )

  out <- bind_rows(event_data, event_end) %>%
    arrange(PatientDurableKey, event_time) %>%
    mutate(status = if_else(event == "end_followup", 0L, 1L)) %>%
    group_by(PatientDurableKey) %>%
    mutate(
      start     = lag(event_time, default = 0),
      stop      = event_time,
      event_num = row_number(),
      gap_start = 0,
      gap       = stop - start
    ) %>%
    ungroup() %>%
    left_join(
      matched.data %>%
        dplyr::select(PatientDurableKey, treatment, treatment_name, all_of(covariates)),
      by = "PatientDurableKey"
    )

  # Fit PWP gap time Cox model
  formula_obj <- as.formula(paste0(
    "Surv(gap_start, gap, status) ~ treatment + ",
    paste(covariates, collapse = " + "),
    " + strata(event_num) + cluster(PatientDurableKey)"
  ))

  fit <- coxph(formula_obj, data = out)

  pwp_result <- list(
    fit                = fit,
    out                = out,
    dep_var            = dep_var,
    comparator_group   = comparator_group,
    target_drug        = target_drug,
    period_name        = period_name,
    bgn_win            = bgn_win,
    end_win            = end_win,
    study_cohort_label = study_cohort_label,
    covariates         = covariates
  )

  save(pwp_result, file = output_file)
  invisible(pwp_result)
}
