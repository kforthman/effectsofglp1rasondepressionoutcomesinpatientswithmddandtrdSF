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

return(
  list(
    target_drug        = target_drug,
    n_sema             = n_sema,
    n_comp             = n_comp,
    match_ratio        = match_ratio,
    tfe_dist           = tfe_dist,
    nonswitch_selected  = nonswitch_selected,
    nonswitch_selected2 = nonswitch_selected2,
    dist_comp          = dist_comp,
    ks_tfe             = ks_tfe,
    ks_age             = ks_age,
    ks_tdi             = ks_tdi,
    ks_year            = ks_year,
    uniqueness_check   = length(unique(nonswitch_selected$PatientDurableKey)) == nrow(nonswitch_selected)
  )
)