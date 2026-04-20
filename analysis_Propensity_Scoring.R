# -*- coding: utf-8 -*-
# Consolidated propensity scoring function for Semaglutide vs. comparator analyses.
#
# prerequisite:
#   - dte_cohort_wNontreat_data.rds
#   - ps_covariates.csv
#
# Usage:
#   source("analysis_Propensity_Scoring.R")
#   result <- analysis_Propensity_Scoring(comparator_group = "DPP4i",
#                                    cohort_file     = "dte_cohort_wNontreat_data.rds",
#                                    covariates_file = "ps_covariates.csv")
#
# Outputs (written to disk by main.R):
#   PS_Covariates-{drug}.csv
#   PS_Weighted_Dataset-{drug}.rds
#   PS_Matched_Dataset-{drug}.rds
#   Data/propensity_scoring_result-{target}Vs{drug}.rds

# ---------------------------------------------------------------------------
# Helper functions for balance assessment
# ---------------------------------------------------------------------------

stat_smd <- function(var, treat, data) {
    z <- data[[treat]]
    x <- data[[var]]
    xT <- x[z == 1]
    xC <- x[z == 0]
    m0 <- mean(xC)
    m1 <- mean(xT)
    s_pooled <- sqrt((sd(xC)^2 + sd(xT)^2) / 2)
    return((m1 - m0) / s_pooled)
}

stat_pval <- function(var, treat, data) {
    z <- data[[treat]]
    x <- data[[var]]
    xT <- x[z == 1]
    xC <- x[z == 0]
    if (is.numeric(x)) {
        test_obj <- try(t.test(xT, xC), silent = TRUE)
        if (inherits(test_obj, "htest")) test_obj$p.value else NA_real_
    } else {
        tab     <- table(x, z)
        chi_obj <- try(chisq.test(tab, correct = FALSE), silent = TRUE)
        if (inherits(chi_obj, "htest") && all(chi_obj$expected >= 5)) {
            chi_obj$p.value
        } else {
            fish_obj <- try(fisher.test(tab), silent = TRUE)
            if (inherits(fish_obj, "htest")) fish_obj$p.value else NA_real_
        }
    }
}

std_diff <- function(var, treat, data) {
    z <- data[[treat]]
    x <- data[[var]]
    xT <- x[z == 1]
    xC <- x[z == 0]
    if (is.numeric(x)) {
        s_pooled <- sqrt((sd(xT, na.rm = TRUE)^2 + sd(xC, na.rm = TRUE)^2) / 2)
        if (s_pooled == 0) return(NA_real_)
        return((mean(xT, na.rm = TRUE) - mean(xC, na.rm = TRUE)) / s_pooled)
    } else {
        pT     <- mean(as.numeric(xT == levels(x)[2]), na.rm = TRUE)
        pC     <- mean(as.numeric(xC == levels(x)[2]), na.rm = TRUE)
        p_pool <- (pT * length(xT) + pC * length(xC)) / (length(xT) + length(xC))
        denom  <- sqrt(p_pool * (1 - p_pool))
        if (denom == 0) return(NA_real_)
        return((pT - pC) / denom)
    }
}

stat_ks <- function(var, treat, data) {
    z <- data[[treat]]
    x <- data[[var]]
    xT <- x[z == 1]
    xC <- x[z == 0]
    if (is.numeric(x)) {
        ks_obj <- try(suppressWarnings(ks.test(xT, xC)), silent = TRUE)
    } else {
        ks_obj <- try(suppressWarnings(ks.test(as.numeric(factor(xT)),
                                               as.numeric(factor(xC)))), silent = TRUE)
    }
    if (inherits(ks_obj, "htest")) {
        list(ks_stat = as.numeric(ks_obj$statistic), ks_pval = ks_obj$p.value)
    } else {
        list(ks_stat = NA_real_, ks_pval = NA_real_)
    }
}

# ---------------------------------------------------------------------------
# Main propensity scoring function
# ---------------------------------------------------------------------------

analysis_Propensity_Scoring <- function(comparator_group,
                                   target_drug     = "Semaglutide",
                                   cohort_file     = "OutputData/dte_cohort_wNontreat_data.rds",
                                   covariates_file = "Data/ps_covariates.csv") {

    # ── 1. Load and prepare data ─────────────────────────────────────────────

    load(cohort_file)
    dte_cohort_data <- this.data

    logical_vars <- dte_cohort_data %>%
        sapply(class) %>%
        as.data.frame() %>%
        rename("type" = ".") %>%
        rownames_to_column("var") %>%
        filter(type == "logical") %>%
        pull(var)

    dte_cohort_data <- dte_cohort_data %>%
        mutate(across(all_of(logical_vars), ~ as.numeric(.)))

    matchingVars    <- read.csv(covariates_file) %>% mutate(issues = NA)
    matchingFormula <- as.formula(paste0("treatment ~ ", paste(matchingVars$var, collapse = " + ")))

    target_pop_colname     <- paste0(target_drug, "_Population_for_", target_drug, "_vs_", comparator_group)
    comparator_pop_colname <- paste0(comparator_group, "_Population_for_", target_drug, "_vs_", comparator_group)

    varname_target_age_at_index_years     <- paste0(target_drug, "_age_at_index_years")
    varname_comparator_age_at_index_years <- paste0(comparator_group, "_age_at_index_years")

    varname_target_mdd_to_index_days     <- paste0(target_drug, "_mdd_to_index_days")
    varname_comparator_mdd_to_index_days <- paste0(comparator_group, "_mdd_to_index_days")

    varname_target_index     <- paste0(target_drug, "_Index")
    varname_comparator_index <- paste0(comparator_group, "_Index")

    this.data <- dte_cohort_data %>%
        filter(!!sym(target_pop_colname) |
               !!sym(comparator_pop_colname)) %>%
        mutate(treatment = ifelse(!!sym(target_pop_colname) == TRUE, 1, 0)) %>%
        mutate(age_at_index_years = ifelse(treatment,
                                           !!sym(varname_target_age_at_index_years),
                                           !!sym(varname_comparator_age_at_index_years))) %>%
        mutate(mdd_to_index_days = ifelse(treatment,
                                          !!sym(varname_target_mdd_to_index_days),
                                          !!sym(varname_comparator_mdd_to_index_days))) %>%
        mutate(index = as.Date(ifelse(treatment,
                                                  !!sym(varname_target_index),
                                                  !!sym(varname_comparator_index))),
               index_year = year(index)) %>%
        as.data.frame()

    # ── 2. Variable selection ────────────────────────────────────────────────

    # 2.1 Near zero variance
    nzv      <- nearZeroVar(this.data[, matchingVars$var], saveMetrics = TRUE)
    nzv_vars <- rownames(nzv)[nzv$nzv]

    matchingVars.1 <- matchingVars %>%
        mutate(issues = ifelse(var %in% nzv_vars,
                               ifelse(is.na(issues), "near zero variance",
                                      paste0(issues, ", near zero variance")),
                               issues))

    # 2.2 Zero variance
    zv_vars <- this.data %>%
        dplyr::select(all_of(matchingVars$var)) %>%
        sapply(function(x) length(unique(x))) %>%
        as.data.frame() %>%
        rownames_to_column("var") %>%
        dplyr::rename(n_unique = ".") %>%
        arrange(n_unique) %>%
        filter(n_unique < 2) %>%
        pull(var)

    matchingVars.2 <- matchingVars.1 %>%
        mutate(issues = ifelse(var %in% zv_vars,
                               ifelse(is.na(issues), "zero variance",
                                      paste0(issues, ", zero variance")),
                               issues))

    # 2.3 Linear combinations
    combos  <- findLinearCombos(this.data %>%
                    dplyr::select(matchingVars %>%
                                  filter(var_type %in% c("continuous", "binary")) %>%
                                  pull(var)))
    lc_vars <- matchingVars$var[combos$remove]

    matchingVars.3 <- matchingVars.2 %>%
        mutate(issues = ifelse(var %in% lc_vars,
                               ifelse(is.na(issues), "linear combination with another variable",
                                      paste0(issues, ", linear combination with another variable")),
                               issues))

    # 2.4 High correlation
    cor_mat <- cor(
        this.data %>%
            dplyr::select(matchingVars %>%
                          filter(var_type %in% c("continuous", "binary")) %>%
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

    for (this_var in matchingVars %>% filter(!var_type == "continuous") %>% pull(var)) {
        this_table <- table(this.data[, c("treatment", this_var)], useNA = "ifany")
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
    vars_negligible_imbalance <- matchingVars %>%
        rowwise() %>%
        mutate(smd = ifelse(var_type %in% c("continuous", "binary"),
                            round(stat_smd(var, "treatment", this.data), 2),
                            NA)) %>%
        filter(abs(smd) < 0.05) %>%
        pull(var)

    matchingVars.6 <- matchingVars.5 %>%
        mutate(issues = ifelse(var %in% vars_negligible_imbalance,
                               ifelse(is.na(issues), "negligible imbalance",
                                      paste0(issues, ", negligible imbalance")),
                               issues))

    vars_no_predict <- matchingVars %>%
        rowwise() %>%
        mutate(pval = round(stat_pval(var, "treatment", this.data), 2)) %>%
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


    # ── 3. Propensity scoring (twang GBM) ────────────────────────────────────

    set.seed(123)

    ps.out <- twang::ps(
      matchingFormula,
      data              = this.data,
      estimand          = "ATT",
      n.trees           = 5000,
      shrinkage         = 0.01,
      interaction.depth = 2,
      bag.fraction      = 0.8,
      n.minobsinnode    = 10,
      stop.method       = c("es.mean", "ks.max"),
      version           = "xgboost",
      verbose           = FALSE
    )

    this.data.ps              <- this.data %>%
        mutate(treatment_name = ifelse(treatment, target_drug, comparator_group))
    this.data.ps$weight       <- get.weights(ps.out, stop.method = "es.mean")
    this.data.ps$pscore       <- ps.out$ps$es.mean.ATT
    this.data.ps$logit_pscore <- log(this.data.ps$pscore / (1 - this.data.ps$pscore))

    # ── 4. IPTW weighted dataset ─────────────────────────────────────────────

    weighted.data <- this.data.ps

    sema_weight_sum <- sum(this.data.ps %>%
                           filter(treatment_name == target_drug) %>% pull(weight))
    alte_weight_sum <- sum(this.data.ps %>%
                           filter(treatment_name == comparator_group) %>% pull(weight))

    total_n_alternate_weighted   <- this.data.ps %>%
        filter(treatment_name == comparator_group) %>% nrow()
    total_small_weights_weighted <- this.data.ps %>%
        filter(treatment_name == comparator_group & weight < 0.01) %>% nrow()
    total_big_weights_weighted   <- this.data.ps %>%
        filter(treatment_name == comparator_group & weight > 0.99) %>% nrow()

    design.ps <- svydesign(ids = ~PatientDurableKey, weights = ~weight, data = this.data.ps)

    # ── 5. Nearest-neighbour matched dataset ─────────────────────────────────

    n_control   <- sum(this.data.ps$treatment == 0)
    n_treatment <- sum(this.data.ps$treatment == 1)
    
    raw_ratio <- max(n_control, n_treatment) / min(n_control, n_treatment)
    
    this_ratio <- ifelse(raw_ratio > 4, 2, 1)
    
    this.data.ps$treatment_flipped <- 1 - this.data.ps$treatment
    
    if(n_control < n_treatment){
      m.out <- matchit(
        treatment_flipped ~ logit_pscore,
        data    = this.data.ps,
        method  = "nearest",
        replace = FALSE,
        ratio   = this_ratio
      )
    }else{
      m.out <- matchit(
        treatment ~ logit_pscore,
        data    = this.data.ps,
        method  = "nearest",
        replace = FALSE,
        ratio   = this_ratio
      )
    }
    
    matched.data <- match.data(m.out)

    total_n_alternate_matched   <- matched.data %>%
        filter(treatment_name == comparator_group) %>% nrow()
    total_small_weights_matched <- matched.data %>%
        filter(treatment_name == comparator_group & weight < 0.01) %>% nrow()
    total_big_weights_matched   <- matched.data %>%
        filter(treatment_name == comparator_group & weight > 0.99) %>% nrow()

    # ── 6. Matched-data balance table ────────────────────────────────────────

    results_list <- vector("list", nrow(matchingVars))
    for (i in 1:nrow(matchingVars)) {
        vname     <- matchingVars[[i, 1]]
        inc_in_ps <- vname %in% matchingVars.final$var

        xT <- matched.data[[vname]][matched.data$treatment == 1]
        xC <- matched.data[[vname]][matched.data$treatment == 0]

        if (is.numeric(matched.data[[vname]])) {
            meanT <- mean(xT, na.rm = TRUE)
            meanC <- mean(xC, na.rm = TRUE)
        } else {
            meanT <- mean(as.numeric(xT == levels(matched.data[[vname]])[2]), na.rm = TRUE)
            meanC <- mean(as.numeric(xC == levels(matched.data[[vname]])[2]), na.rm = TRUE)
        }

        ks_out <- stat_ks(vname, "treatment", matched.data)

        results_list[[i]] <- data.frame(
            Covariate      = vname,
            Included_in_PS = inc_in_ps,
            Mean_Treated   = round(meanT, 3),
            Mean_Control   = round(meanC, 3),
            Raw_Difference = round(meanT - meanC, 3),
            Std_Difference = round(std_diff(vname, "treatment", matched.data), 3),
            Stat_P_Value   = round(stat_pval(vname, "treatment", matched.data), 4),
            KS_Stat        = round(ks_out$ks_stat, 4),
            KS_Pvalue      = round(ks_out$ks_pval, 4),
            stringsAsFactors = FALSE
        )
    }

    balance_table            <- do.call(rbind, results_list)
    row.names(balance_table) <- NULL

    # ── Save and return all results ──────────────────────────────────────────

    result <- list(
        target_drug                  = target_drug,
        comparator_group              = comparator_group,
        this.data                    = this.data,
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
        matchingFormula              = matchingFormula,
        ps.out                       = ps.out,
        this.data.ps                 = this.data.ps,
        design.ps                    = design.ps,
        weighted.data                = weighted.data,
        sema_weight_sum              = sema_weight_sum,
        alte_weight_sum              = alte_weight_sum,
        total_n_alternate_weighted   = total_n_alternate_weighted,
        total_small_weights_weighted = total_small_weights_weighted,
        total_big_weights_weighted   = total_big_weights_weighted,
        m.out                        = m.out,
        matched.data                 = matched.data,
        balance_table                = balance_table,
        total_n_alternate_matched    = total_n_alternate_matched,
        total_small_weights_matched  = total_small_weights_matched,
        total_big_weights_matched    = total_big_weights_matched
    )

    result
}
