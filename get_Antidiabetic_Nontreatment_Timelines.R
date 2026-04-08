# -*- coding: utf-8 -*-
# get_Antidiabetic_Nontreatment_Timelines(): Defines periods of time for which participants
# do not experience a change in antidiabetic treatment. Only use of a new drug
# counts as a change. Eligible time is masked before 6 months before MDD first
# diagnosis and after beginning target_drug.
#
# Arguments:
#   dte_cohort_data   — Treatment episode data (from Treatment_Table CSV,
#                       already joined to mdd_data and filtered to
#                       Eligibility_Group_B)
#   nonswitch_periods — Nontreatment period data (from Nontreatment_Table CSV,
#                       already joined to mdd_data, filtered to
#                       Eligibility_Group_B, and with tfe_at_index_bgn /
#                       tfe_at_index_end computed)
#   target_drug        — Name of target treatment (default: "Semaglutide")
#   nontreatment_group — Name of the nontreatment group (default: "Nontreatment")
#   mdd_data           — Patient-level MDD data (used to populate demographic
#                        columns for nontreatment-only patients added as new rows)
#
# Returns:
#   List of diagnostic objects for report_Antidiabetic_Nontreatment_Timelines.Rmd,
#   plus dte_cohort_data2 (saved to disk by main.R as Data/dte_cohort_wNontreat_data.rds)

get_Antidiabetic_Nontreatment_Timelines <- function(dte_cohort_data,
                                                    nonswitch_periods,
                                                    target_drug = "Semaglutide",
                                                    nontreatment_group = "Nontreatment",
                                                    mdd_data) {
  
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
  
  # ── Distribution of time from eligibility to index ───────────────────────
  
  tfe_dist <- target_data %>%
    count(!!sym(col_mdd_to_index)) %>%
    rename(tfe_at_index_days = !!sym(col_mdd_to_index)) %>%
    mutate(freq = n / sum(n))
  
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
  
  # ── Build distribution comparison for KS test ────────────────────────────
  
  tfe_dist_wide <- tfe_dist %>% dplyr::select(-freq)
  colnames(tfe_dist_wide)[colnames(tfe_dist_wide) == "n"] <- target_drug
  
  dist_comp <- tfe_dist_wide %>%
    left_join(
      nonswitch_selected %>%
        count(emu_tfe_at_index_days) %>%
        rename(Comparison       = n,
               tfe_at_index_days = emu_tfe_at_index_days),
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
  # Patients in the nontreatment arm who never had any antidiabetic drug
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
  
  # ── KS tests for final diagnostic distributions ──────────────────────────
  
  s_samp_age <- dte_cohort_data3 %>% filter(treatment_name == target_drug)     %>% pull(age_at_index_years)
  c_samp_age <- dte_cohort_data3 %>% filter(treatment_name == nontreatment_group) %>% pull(age_at_index_years)
  ks_age     <- ks.test(s_samp_age, c_samp_age)
  
  s_samp_tdi <- dte_cohort_data3 %>% filter(treatment_name == target_drug)     %>% pull(time_diag_to_index_days)
  c_samp_tdi <- dte_cohort_data3 %>% filter(treatment_name == nontreatment_group) %>% pull(time_diag_to_index_days)
  ks_tdi     <- ks.test(s_samp_tdi, c_samp_tdi)
  
  s_samp_yr  <- dte_cohort_data3 %>% filter(treatment_name == target_drug)     %>% pull(index_year)
  c_samp_yr  <- dte_cohort_data3 %>% filter(treatment_name == nontreatment_group) %>% pull(index_year)
  ks_year    <- ks.test(s_samp_yr, c_samp_yr)
  
  # ── Return diagnostic results for reporting ───────────────────────────────
  
  list(
    dte_cohort_data2   = dte_cohort_data2,
    target_drug        = target_drug,
    n_sema             = n_sema,
    n_comp             = n_comp,
    match_ratio        = match_ratio,
    tfe_dist           = tfe_dist,
    nonswitch_selected  = nonswitch_selected,
    nonswitch_selected2 = nonswitch_selected2,
    dist_comp          = dist_comp,
    ks_tfe             = ks_tfe,
    dte_cohort_data3   = dte_cohort_data3,
    ks_age             = ks_age,
    ks_tdi             = ks_tdi,
    ks_year            = ks_year,
    uniqueness_check   = length(unique(nonswitch_selected$PatientDurableKey)) == nrow(nonswitch_selected)
  )
}
