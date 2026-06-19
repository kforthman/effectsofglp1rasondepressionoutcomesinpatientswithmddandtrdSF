# -*- coding: utf-8 -*-
# get_Nontreatment_Timelines(): Defines periods of time for which participants
# do not experience a change in treatment. Only use of a new treatment drug
# counts as a change. Eligible time is masked before 6 months before MDD first
# diagnosis and after beginning target_drug.
#
# Arguments:
#   dte_cohort_data_file   — Path to RDS file containing treatment episode data (from Treatment_Table CSV,
#                       already joined to mdd_data and filtered to
#                       Eligibility_Group_B)
#   nonswitch_periods_file — Path to RDS file containing nontreatment period data (from Nontreatment_Table CSV,
#                       already joined to mdd_data, filtered to
#                       Eligibility_Group_B, and with tfe_at_index_bgn /
#                       tfe_at_index_end computed)
#   target_drug        — Name of target treatment (default: "Semaglutide")
#   nontreatment_group — Name of the nontreatment group (default: "Nontreatment")
#   mdd_data_file           — Path to RDS file containing patient-level MDD data (used to populate demographic
#                        columns for nontreatment-only patients added as new rows)
#   nontreat_data_filename - Filename for nontreatment data
#   result_file            - Filename for results file


get_Nontreatment_Summary   <- function(dte_cohort_wNontreat_data,
                                       n_sema,
                                       n_comp,
                                       target_drug = "Semaglutide",
                                       nontreatment_group = "Nontreatment",
                                       tfe_dist) {

  # ── Nontreatment eligible population ─────────────────────────────────────
  
  match_ratio <- floor(n_comp / n_sema)
  
  # ── Build distribution comparison for KS test ────────────────────────────
  
  tfe_dist_wide <- tfe_dist %>% dplyr::select(-freq)
  colnames(tfe_dist_wide)[colnames(tfe_dist_wide) == "n"] <- target_drug

  dist_comp <- tfe_dist_wide %>%
    left_join(
      dte_cohort_wNontreat_data %>%
        filter(treatment_name == nontreatment_group) %>%
        count(mdd_to_index_days) %>%
        rename(Comparison        = n,
               tfe_at_index_days = mdd_to_index_days),
      by = join_by(tfe_at_index_days == tfe_at_index_days)
    ) %>%
    pivot_longer(all_of(c(target_drug, "Comparison")),
                 names_to  = "group",
                 values_to = "n",
                 values_drop_na = TRUE)
  
  s_samp_tfe <- dist_comp %>%
    filter(group == target_drug) %>%
    dplyr::select(-group) %>%
    uncount(weights = n) %>%
    pull(tfe_at_index_days)

  c_samp_tfe <- dist_comp %>%
    filter(group == "Comparison") %>%
    dplyr::select(-group) %>%
    uncount(weights = n) %>%
    pull(tfe_at_index_days)

  ks_tfe <- ks.test(s_samp_tfe, c_samp_tfe)
  
  # ── KS tests for final diagnostic distributions ──────────────────────────
  
  s_samp_age <- dte_cohort_wNontreat_data %>% filter(treatment_name == target_drug)        %>% pull(age_at_index_years)
  c_samp_age <- dte_cohort_wNontreat_data %>% filter(treatment_name == nontreatment_group) %>% pull(age_at_index_years)
  ks_age     <- ks.test(s_samp_age, c_samp_age)

  s_samp_tdi <- dte_cohort_wNontreat_data %>% filter(treatment_name == target_drug)        %>% pull(mdd_to_index_days)
  c_samp_tdi <- dte_cohort_wNontreat_data %>% filter(treatment_name == nontreatment_group) %>% pull(mdd_to_index_days)
  ks_tdi     <- ks.test(s_samp_tdi, c_samp_tdi)

  s_samp_yr  <- dte_cohort_wNontreat_data %>% filter(treatment_name == target_drug)        %>% pull(index_year)
  c_samp_yr  <- dte_cohort_wNontreat_data %>% filter(treatment_name == nontreatment_group) %>% pull(index_year)
  ks_year    <- ks.test(s_samp_yr, c_samp_yr)
  
  # ── Uniqueness check                        ───────────────────────────────
  
  
  nontreat_ids <- dte_cohort_wNontreat_data %>% filter(treatment_name == nontreatment_group) %>% pull(PatientDurableKey)
  uniqueness_check <- length(unique(nontreat_ids)) == length(nontreat_ids)
  
  # ── Return diagnostic results for reporting ───────────────────────────────
  
  return(
    list(
      target_drug        = target_drug,
      nontreatment_group = nontreatment_group,
      n_sema             = n_sema,
      n_comp             = n_comp,
      match_ratio        = match_ratio,
      tfe_dist           = tfe_dist,
      dist_comp          = dist_comp,
      ks_tfe             = ks_tfe,
      ks_age             = ks_age,
      ks_tdi             = ks_tdi,
      ks_year            = ks_year,
      uniqueness_check   = uniqueness_check
    )
  )
}
