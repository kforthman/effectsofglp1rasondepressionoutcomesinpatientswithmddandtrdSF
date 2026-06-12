analysis_Propensity_Scoring_Variable_Selection <- function(comparator_group,
                                                           target_drug     = "Semaglutide",
                                                           cohort_table     = analysis_data,
                                                           covariates_table = ps_covariates) {
  

  
  matchingVars    <- covariates_table %>% mutate(issues = NA)
  matchingFormula <- as.formula(paste0("treatment ~ ", paste(matchingVars$var, collapse = " + ")))
  
  # ── 1. Load and prepare data ─────────────────────────────────────────────
  
  logical_vars <- cohort_table %>%
    dplyr::select(all_of(matchingVars$var)) %>%
    sapply(class) %>%
    as.data.frame() %>%
    rename("type" = ".") %>%
    rownames_to_column("var") %>%
    filter(type == "logical") %>%
    pull(var)
  
  cohort_table <- cohort_table %>%
    mutate(across(all_of(logical_vars), ~ as.numeric(.)))
  
  # ── 2. Variable selection ────────────────────────────────────────────────
  
  # 2.1 Zero variance
  zv_vars <- cohort_table %>%
    dplyr::select(all_of(matchingVars$var)) %>%
    sapply(function(x) length(unique(x))) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    dplyr::rename(n_unique = ".") %>%
    arrange(n_unique) %>%
    filter(n_unique < 2) %>%
    pull(var)
  
  matchingVars.1 <- matchingVars %>%
    mutate(issues = ifelse(var %in% zv_vars,
                           ifelse(is.na(issues), "zero variance",
                                  paste0(issues, ", zero variance")),
                           issues))
  
  # 2.2 Near zero variance
  nzv      <- nearZeroVar(cohort_table[, matchingVars.1$var], saveMetrics = TRUE)
  nzv_vars <- rownames(nzv)[nzv$nzv]
  
  matchingVars.2 <- matchingVars.1 %>%
    mutate(issues = ifelse(var %in% nzv_vars,
                           ifelse(is.na(issues), "near zero variance",
                                  paste0(issues, ", near zero variance")),
                           issues))
  
  # 2.3 Linear combinations
  combos  <- findLinearCombos(cohort_table %>%
                                dplyr::select(matchingVars.2 %>%
                                                filter(var_type %in% c("continuous", "binary") &
                                                         is.na(issues)) %>%
                                                pull(var)))
  lc_vars <- matchingVars.2$var[combos$remove]
  
  matchingVars.3 <- matchingVars.2 %>%
    mutate(issues = ifelse(var %in% lc_vars,
                           ifelse(is.na(issues), "linear combination with another variable",
                                  paste0(issues, ", linear combination with another variable")),
                           issues))
  
  # 2.4 High correlation
  cor_mat <- cor(
    cohort_table %>%
      dplyr::select(matchingVars.3 %>%
                      filter(var_type %in% c("continuous", "binary") &
                               is.na(issues)) %>%
                      pull(var)),
    use = "pairwise.complete.obs"
  )
  
  these_cors <- which(abs(cor_mat) > 0.8, arr.ind = TRUE, useNames = TRUE) %>%
    as.data.frame() %>%
    left_join(data.frame(row = 1:nrow(cor_mat), row.name = rownames(cor_mat)),
              by = join_by(row == row)) %>%
    left_join(data.frame(col = 1:ncol(cor_mat), col.name = colnames(cor_mat)),
              by = join_by(col == col)) %>%
    filter(!row == col) %>%
    mutate(correlated = col.name) %>%
    rename(var = row.name) %>%
    dplyr::select(var, correlated)
  
  hc_vars <- these_cors %>% pull(var)
  
  matchingVars.4 <- matchingVars.3 %>%
    mutate(issues = ifelse(var %in% hc_vars,
                           ifelse(is.na(issues), "highly correlated with another variable",
                                  paste0(issues, ", highly correlated with another variable")),
                           issues)) %>%
    left_join(these_cors)
  
  # 2.5 Positivity
  positivity_tables <- list()
  pos_fail_vars     <- c()
  
  for (this_var in matchingVars.4 %>% filter(!var_type == "continuous" & is.na(issues)) %>% pull(var)) {
    this_table <- table(cohort_table[, c("treatment", this_var)], useNA = "ifany")
    this_name  <- colnames(as.data.frame(this_table))[2]
    this_col   <- sym(this_name)
    
    tbl_wide <- this_table %>%
      as.data.frame() %>%
      mutate(!!this_col := as.character(!!this_col)) %>%
      pivot_wider(names_from  = !!this_col,
                  values_from = Freq,
                  names_prefix = paste0(this_name, "_")) %>%
      mutate(across(2:3, .fns = ~ ifelse(.x < 20, "<20", .x)))
    
    positivity_tables[[this_var]] <- tbl_wide
    if (sum(this_table == 0) > 0) pos_fail_vars <- c(pos_fail_vars, this_var)
  }
  
  matchingVars.5 <- matchingVars.4 %>%
    mutate(issues = ifelse(var %in% pos_fail_vars,
                           ifelse(is.na(issues), "fails positivity assumption",
                                  paste0(issues, ", fails positivity assumption")),
                           issues))
  
  # 2.6 Balance: negligible imbalance (SMD < 0.05) and does not predict treatment (p > 0.05)
  vars_negligible_imbalance <- matchingVars.5 %>%
    rowwise() %>%
    mutate(smd = ifelse(var_type %in% c("continuous", "binary"),
                        round(stat_smd(var, "treatment", cohort_table), 2),
                        NA)) %>%
    filter(abs(smd) < 0.05) %>%
    pull(var)
  
  matchingVars.6 <- matchingVars.5 %>%
    mutate(issues = ifelse(var %in% vars_negligible_imbalance,
                           ifelse(is.na(issues), "negligible imbalance",
                                  paste0(issues, ", negligible imbalance")),
                           issues))
  
  vars_no_predict <- matchingVars.6 %>%
    rowwise() %>%
    mutate(pval = round(stat_pval(var, "treatment", cohort_table), 2)) %>%
    filter(pval > 0.05) %>%
    pull(var)
  
  matchingVars.7 <- matchingVars.6 %>%
    mutate(issues = ifelse(var %in% vars_no_predict,
                           ifelse(is.na(issues), "does not predict treatment",
                                  paste0(issues, ", does not predict treatment")),
                           issues))
  
  # 2.7 Filter: keep only vars with no issues OR only high correlation
  matchingVars.8 <- matchingVars.7 %>%
    filter(is.na(issues) | issues == "highly correlated with another variable")
  
  matchingVars.9 <- matchingVars.8 %>%
    rowwise() %>%
    mutate(issues     = ifelse(!is.na(issues) & !correlated %in% matchingVars.8$var,
                               NA, issues),
           correlated = ifelse(is.na(issues), NA, correlated)) %>%
    ungroup()
  
  matchingVars.10 <- matchingVars.9
  while (sum(matchingVars.10$issues == "highly correlated with another variable",
             na.rm = TRUE) > 0) {
    rm.var <- (matchingVars.10 %>% filter(!is.na(correlated)) %>% pull(correlated))[1]
    matchingVars.10 <- matchingVars.10 %>%
      filter(!var == rm.var) %>%
      mutate(issues     = ifelse(correlated == rm.var, NA, issues),
             correlated = ifelse(correlated == rm.var, NA, correlated))
  }
  
  matchingVars.final <- matchingVars.10 %>% dplyr::select(var, var_type)
  matchingFormula    <- as.formula(paste0("treatment ~ ",
                                          paste(matchingVars.final$var, collapse = " + ")))
  
  # ── Save and return all results ──────────────────────────────────────────
  
  result <- list(
    target_drug                  = target_drug,
    comparator_group             = comparator_group,
    logical_vars                 = logical_vars,
    matchingVars                 = matchingVars,
    nzv                          = nzv,
    nzv_vars                     = nzv_vars,
    matchingVars.1               = matchingVars.1,
    zv_vars                      = zv_vars,
    matchingVars.2               = matchingVars.2,
    lc_vars                      = lc_vars,
    matchingVars.3               = matchingVars.3,
    cor_mat                      = cor_mat,
    these_cors                   = these_cors,
    matchingVars.4               = matchingVars.4,
    positivity_tables            = positivity_tables,
    pos_fail_vars                = pos_fail_vars,
    matchingVars.5               = matchingVars.5,
    vars_negligible_imbalance    = vars_negligible_imbalance,
    matchingVars.6               = matchingVars.6,
    vars_no_predict              = vars_no_predict,
    matchingVars.7               = matchingVars.7,
    matchingVars.8               = matchingVars.8,
    matchingVars.9               = matchingVars.9,
    matchingVars.10              = matchingVars.10,
    matchingVars.final           = matchingVars.final,
    matchingFormula              = matchingFormula
  )
  
  result
}