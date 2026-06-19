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


get_Nontreatment_Timelines <- function(dte_cohort_data,
                                       nonswitch_periods,
                                       target_drug = "Semaglutide",
                                       nontreatment_group = "Nontreatment",
                                       mdd_data,
                                       tfe_dist) {
  
  col_TimelineCriteria <- paste0(target_drug,        "_meets_timeline_criteria")
  col_mdd_to_index    <- paste0(target_drug,        "_mdd_to_index_days")
  col_Index           <- paste0(target_drug,        "_Index")
  col_vs_Nontreat     <- paste0(target_drug,        "_Population_for_", target_drug, "_vs_", nontreatment_group)
  col_Nontreat_vs     <- paste0(nontreatment_group, "_Population_for_", target_drug, "_vs_", nontreatment_group)
  col_age_at_index    <- paste0(target_drug,        "_age_at_index_years")
  
  col_nt_Index          <- paste0(nontreatment_group, "_Index")
  col_nt_mdd_to_index   <- paste0(nontreatment_group, "_mdd_to_index_days")
  col_nt_meets_criteria <- paste0(nontreatment_group, "_meets_timeline_criteria")
  col_nt_age_at_index   <- paste0(nontreatment_group, "_age_at_index_years")
  
  # ── Identify the eligible treatment group ─────────────────────────────────
  
  target_data <- dte_cohort_data %>%
    filter(!!sym(col_TimelineCriteria) == 1)
  
  # ── Nontreatment eligible population ─────────────────────────────────────
  
  nonswitch_eligible <- nonswitch_periods %>%
    filter(!PatientDurableKey %in% target_data$PatientDurableKey)
  
  n_comp      <- nonswitch_eligible %>% pull(PatientDurableKey) %>% unique() %>% length()
  n_sema      <- nrow(target_data)
  match_ratio <- floor(n_comp / n_sema)
  
  # ── Index date emulation ──────────────────────────────────────────────────
  
  set.seed(2025)
  tfe_at_index <- data.frame(
    PatientDurableKey     = unique(nonswitch_eligible$PatientDurableKey),
    emu_tfe_at_index_days = sample(
      x       = tfe_dist$tfe_at_index_days,
      size    = n_comp,
      replace = TRUE,
      prob    = tfe_dist$freq
    )
  )
  
  nonswitch_selected <- nonswitch_eligible %>%
    left_join(tfe_at_index, by = "PatientDurableKey") %>%
    filter(tfe_at_index_bgn <= emu_tfe_at_index_days &
             tfe_at_index_end >= emu_tfe_at_index_days)
  
  # ── Finalize: assign nontreatment index dates ─────────────────────────────
  
  nonswitch_selected2 <- nonswitch_selected %>%
    mutate(!!col_nt_Index        := MDD_Index + days(emu_tfe_at_index_days)) %>%
    rename(!!col_nt_mdd_to_index := "emu_tfe_at_index_days")
  
  # ── Combine into main cohort dataset ─────────────────────────────────────
  
  dte_cohort_data2 <- dte_cohort_data %>%
    left_join(
      nonswitch_selected2 %>% dplyr::select(PatientDurableKey, all_of(c(col_nt_Index, col_nt_mdd_to_index))),
      by = "PatientDurableKey"
    ) %>%
    mutate(
      !!col_nt_meets_criteria                   := PatientDurableKey %in% nonswitch_selected2$PatientDurableKey,
      !!col_vs_Nontreat                         := PatientDurableKey %in% target_data$PatientDurableKey,
      !!col_Nontreat_vs                         := PatientDurableKey %in% nonswitch_selected2$PatientDurableKey,
      !!col_nt_age_at_index                     := time_length(interval(BirthDate, !!sym(col_nt_Index)), "years")
    )
  
  # ── Add nontreatment-only patients as new rows ───────────────────────────
  # Patients in the nontreatment arm who never had any treatment drug
  # treatment episode are not in dte_cohort_data and must be added explicitly.
  
  nontreat_only <- nonswitch_selected2 %>%
    filter(!PatientDurableKey %in% dte_cohort_data$PatientDurableKey) %>%
    distinct(PatientDurableKey, .keep_all = TRUE) %>%
    dplyr::select(PatientDurableKey, all_of(c(col_nt_Index, col_nt_mdd_to_index))) %>%
    left_join(mdd_data, by = "PatientDurableKey") %>%
    mutate(
      !!col_nt_meets_criteria  := TRUE,
      !!col_vs_Nontreat        := FALSE,
      !!col_Nontreat_vs        := TRUE,
      !!col_nt_age_at_index    := time_length(interval(BirthDate, !!sym(col_nt_Index)), "years")
    )
  
  dte_cohort_data2 <- bind_rows(dte_cohort_data2, nontreat_only)
  
  na_logical_cols <- dte_cohort_data2 %>%
    dplyr::select(where(is.logical)) %>%
    dplyr::select(where(~ any(is.na(.)))) %>%
    names()
  
  if (length(na_logical_cols) > 0) {
    message("The following logical columns contain NA for nontreatment-only rows and will be set to FALSE:\n  ",
            paste(na_logical_cols, collapse = "\n  "))
  }
  
  dte_cohort_data2 <- dte_cohort_data2 %>%
    mutate(across(where(is.logical), ~ replace_na(., FALSE)))
  
  # ── Diagnostic dataset for reporting ─────────────────────────────────────
  
  dte_cohort_data3 <- dte_cohort_data2 %>%
    filter(!!sym(col_vs_Nontreat) | !!sym(col_Nontreat_vs)) %>%
    mutate(treatment      = ifelse(!!sym(col_vs_Nontreat), 1, 0)) %>%
    mutate(treatment_name = ifelse(treatment, target_drug, nontreatment_group)) %>%
    mutate(index_date     = as.Date(ifelse(treatment,
                                           !!sym(col_Index),
                                           !!sym(col_nt_Index)))) %>%
    mutate(index_year     = year(index_date)) %>%
    mutate(time_diag_to_index_days = ifelse(treatment,
                                            !!sym(col_mdd_to_index),
                                            !!sym(col_nt_mdd_to_index))) %>%
    mutate(age_at_index_years      = ifelse(treatment,
                                            !!sym(col_age_at_index),
                                            !!sym(col_nt_age_at_index)))

  # ── Return diagnostic results for reporting ───────────────────────────────
  
  return(
    list(
      dte_cohort_data2   = dte_cohort_data2,
      dte_cohort_data3   = dte_cohort_data3
    )
  )
}
