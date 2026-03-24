# -*- coding: utf-8 -*-
# get_Antidiabetic_Nontreatment_Timelines(): Defines periods of time for which participants
# do not experience a change in antidiabetic treatment. Only use of a new drug
# counts as a change. Eligible time is masked before 6 months before MDD first
# diagnosis and after beginning target_drug.
#
# Arguments:
#   mdd_data          — MDD patient demographics (from MDDPatientList CSV)
#   dte_cohort_data   — Treatment episode data (from Treatment_Table CSV,
#                       already joined to mdd_data and filtered to
#                       Eligibility_Group_B)
#   nonswitch_periods — Nontreatment period data (from Nontreatment_Table CSV,
#                       already joined to mdd_data, filtered to
#                       Eligibility_Group_B, and with tfe_at_index_bgn /
#                       tfe_at_index_end computed)
#   target_drug       — Name of target treatment (default: "Semaglutide")
#
# Returns:
#   List of diagnostic objects for report_Antidiabetic_Nontreatment_Timelines.Rmd,
#   plus dte_cohort_data2 (saved to disk by main.R as Data/dte_cohort_wNontreat_data.rds)

get_Antidiabetic_Nontreatment_Timelines <- function(mdd_data,
                                       dte_cohort_data,
                                       nonswitch_periods,
                                       target_drug = "Semaglutide") {

    col_PrePostCriteria <- paste0(target_drug, "_PrePostInclusionCriteriaMet")
    col_MDD_before      <- paste0("MDD_before_", target_drug)
    col_mdd_to_index    <- paste0(target_drug, "_mdd_to_index_days")
    col_Index           <- paste0(target_drug, "_Index")
    col_vs_Nontreat     <- paste0(target_drug, "_vs_Nontreatment_AllCriteriaMet")
    col_Nontreat_vs     <- paste0("Nontreatment_vs_", target_drug, "_AllCriteriaMet")
    col_age_at_index    <- paste0(target_drug, "_age_at_index_years")

    # ── Identify the eligible treatment group ─────────────────────────────────

    target_data <- dte_cohort_data %>%
        filter(!!sym(col_PrePostCriteria) == 1 & !!sym(col_MDD_before) == 1) %>%
        mutate(!!col_mdd_to_index := as.numeric(
            difftime(!!sym(col_Index), MDD_Index, units = "days")
        ))

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
        mutate(Nontreatment_Index = MDD_Index + days(emu_tfe_at_index_days)) %>%
        rename(Nontreatment_mdd_to_index_days = "emu_tfe_at_index_days")

    # ── Combine into main cohort dataset ─────────────────────────────────────

    dte_cohort_data2 <- dte_cohort_data %>%
        left_join(
            nonswitch_selected2 %>% dplyr::select(PatientDurableKey, Nontreatment_Index),
            by = "PatientDurableKey"
        ) %>%
        mutate(
            Nontreatment_Iplus15                     = Nontreatment_Index + days(15),
            Nontreatment_Iplus365                    = Nontreatment_Index + days(365),
            MDD_before_Nontreatment                  = PatientDurableKey %in% nonswitch_selected$PatientDurableKey,
            Nontreatment_PreIndexEncounterCount      = NA,
            Nontreatment_PostIndexEncounterCount     = NA,
            Nontreatment_PrePostInclusionCriteriaMet = PatientDurableKey %in% nonswitch_selected$PatientDurableKey,
            !!col_vs_Nontreat                        := PatientDurableKey %in% target_data$PatientDurableKey,
            !!col_Nontreat_vs                        := PatientDurableKey %in% nonswitch_selected$PatientDurableKey
        )

    # ── Diagnostic dataset for reporting ─────────────────────────────────────

    dte_cohort_data3 <- dte_cohort_data2 %>%
        left_join(mdd_data %>% dplyr::select(PatientDurableKey, BirthDate),
                  by = "PatientDurableKey") %>%
        filter(!!sym(col_vs_Nontreat) | !!sym(col_Nontreat_vs)) %>%
        mutate(treatment      = ifelse(!!sym(col_vs_Nontreat), 1, 0)) %>%
        mutate(treatment_name = ifelse(treatment, target_drug, "Nontreatment")) %>%
        mutate(index_date     = as.Date(ifelse(treatment,
                                               !!sym(col_Index),
                                               Nontreatment_Index))) %>%
        mutate(index_year     = year(index_date)) %>%
        mutate(!!col_mdd_to_index := as.numeric(
            difftime(!!sym(col_Index), MDD_Index, units = "days")
        )) %>%
        mutate(Nontreatment_mdd_to_index_days = as.numeric(
            difftime(Nontreatment_Index, MDD_Index, units = "days")
        )) %>%
        mutate(!!col_age_at_index := floor(
            time_length(interval(BirthDate, !!sym(col_Index)), "years")
        )) %>%
        mutate(Nontreatment_age_at_index_years = floor(
            time_length(interval(BirthDate, Nontreatment_Index), "years")
        )) %>%
        mutate(time_diag_to_index_days = ifelse(treatment,
                                                !!sym(col_mdd_to_index),
                                                Nontreatment_mdd_to_index_days)) %>%
        mutate(age_at_index_years      = ifelse(treatment,
                                                !!sym(col_age_at_index),
                                                Nontreatment_age_at_index_years))

    # ── KS tests for final diagnostic distributions ──────────────────────────

    s_samp_age <- dte_cohort_data3 %>% filter(treatment_name == target_drug)     %>% pull(age_at_index_years)
    c_samp_age <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(age_at_index_years)
    ks_age     <- ks.test(s_samp_age, c_samp_age)

    s_samp_tdi <- dte_cohort_data3 %>% filter(treatment_name == target_drug)     %>% pull(time_diag_to_index_days)
    c_samp_tdi <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(time_diag_to_index_days)
    ks_tdi     <- ks.test(s_samp_tdi, c_samp_tdi)

    s_samp_yr  <- dte_cohort_data3 %>% filter(treatment_name == target_drug)     %>% pull(index_year)
    c_samp_yr  <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(index_year)
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
