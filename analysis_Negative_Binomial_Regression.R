# -*- coding: utf-8 -*-
# analysis_Negative_Binomial_Regression(): Fits Poisson and negative binomial
# regression models for a single outcome variable, comparator group, and
# time period.
#
# Arguments:
#   matched_data_file — Path to PS_Matched_Dataset-{group}.rds. Must restore
#                       `matched.data` with columns PatientDurableKey,
#                       treatment, treatment_name, {target_drug}_Index,
#                       {comparator_group}_Index, and covariate columns.
#   all_outcomes      — Long-format outcomes data frame (from main.R) with
#                       columns: PatientDurableKey, study_cohort, period,
#                       var_name, value.
#   period_info       — Data frame with columns: period, period_name,
#                       bgn_win, end_win. Used to map period codes (e.g. "d")
#                       to full period labels.
#   comparator_group  — Comparator group name (e.g. "Insulins").
#   target_drug       — Target drug name (e.g. "Semaglutide").
#   period_name       — Period code matching period_info$period_name (e.g. "d").
#   dep_var           — Outcome variable base name, without period suffix
#                       (e.g. "n_psych_days" or "n_med_changes").
#   covariates        — Character vector of covariate column names
#                       (e.g. c("race.ethnicity_White", "sex_Male",
#                               "age_at_index_years")).
#   output_file       — Path to save the result .rds file.
#
# Returns (invisibly):
#   Named list with model objects, analysis dataset, and metadata.
#   Also saved to output_file as `nb_result`.

analysis_Negative_Binomial_Regression <- function(matched_data_file,
                                                   all_outcomes,
                                                   comparator_group,
                                                   target_drug,
                                                   period_name,
                                                   dep_var,
                                                   covariates,
                                                   output_file) {

  load(matched_data_file)   # restores matched.data

  study_cohort_label <- paste0(target_drug, " vs ", comparator_group)

  matched.data <- matched.data %>%
    mutate(
      index_date   = if_else(
        treatment_name == target_drug,
        !!sym(paste0(target_drug,       "_Index")),
        !!sym(paste0(comparator_group,  "_Index"))
      ),
      study_cohort = study_cohort_label
    )

  # Pivot all_outcomes to wide for this comparator, appending period_name suffix
  outcomes_wide <- all_outcomes %>%
    filter(study_cohort == study_cohort_label) %>%
    filter(period == period_name) %>%
    dplyr::select(-period) %>%
    pivot_wider(names_from = "var_name", values_from = "value")

  analysis_data <- matched.data %>%
    left_join(outcomes_wide, by = c("PatientDurableKey", "study_cohort"))

  # Build formula: {dep_var}_{period_name} ~ treatment + covariate1 + ...
  formula_obj <- as.formula(
    paste0(dep_var, " ~ treatment + ", paste(covariates, collapse = " + "))
  )

  # Fit Poisson (overdispersion check) then negative binomial
  m_pois <- glm(formula_obj, data = analysis_data, family = poisson())
  m1     <- glm.nb(formula_obj, data = analysis_data)

  nb_result <- list(
    m_pois             = m_pois,
    m1                 = m1,
    dataset            = analysis_data,
    dep_var            = dep_var,
    comparator_group   = comparator_group,
    target_drug        = target_drug,
    period_name        = period_name,
    study_cohort_label = study_cohort_label,
    covariates         = covariates
  )

  save(nb_result, file = output_file)
  invisible(nb_result)
}
