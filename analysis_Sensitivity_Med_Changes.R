# -*- coding: utf-8 -*-
# %% [markdown]
# # Setup

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()
source("helper_functions.R")

# %% [markdown]
# # Custom Functions

# %%
interpret_nb <- function(model, term, outcome, comparison, alpha = 0.05, digits = 2) {
    coefs <- summary(model)$coefficients

    if (!term %in% rownames(coefs)) {
        stop(sprintf("Term '%s' not found in model.", term))
    }

    est   <- coefs[term, "Estimate"]
    se    <- coefs[term, "Std. Error"]
    ci_level <- 1 - alpha
    z     <- qnorm(1 - alpha/2)
    lo    <- est - z * se
    hi    <- est + z * se

    rr    <- exp(est)
    rr_lo <- exp(lo)
    rr_hi <- exp(hi)
    pval  <- coefs[term, "Pr(>|z|)"]

    sig_txt <- if (pval < alpha) "This difference is statistically significant" else
        "This difference is not statistically significant"

        direction <- if (rr < 1) "lower" else "higher"
            pct_diff  <- abs(1 - rr) * 100

        cat(
            sprintf("Estimated rate ratio: %0.*f (%d%% CI %0.*f to %0.*f)\n",
                    digits, rr, round(ci_level * 100), digits, rr_lo, digits, rr_hi),
            sprintf(
                "Interpretation: Patients in the group with Semaglutide treatment had about %0.*f%% %s rate of %s compared with %s treatment, adjusting for other variables.\n",
                digits,
                pct_diff,
                direction,
                outcome,
                comparison
            ),
            sprintf("p-value: %0.4f. %s at alpha = %s.\n", pval, sig_txt, alpha),
            sep = ""
        )
    }

# %%
check_od <- function(model, threshold = 0.05) {
    od <- check_overdispersion(model)
    if (od$p_value < threshold)
        cat(sprintf("Overdispersed: ratio = %.2f, p = %.4f\n", od$dispersion_ratio, od$p_value))
    else
        cat(sprintf("Not overdispersed: ratio = %.2f, p = %.4f\n", od$dispersion_ratio, od$p_value))
}

check_mc <- function(model, vif_threshold = 5) {
    cc <- check_collinearity(model)
    max_vif <- max(cc$VIF, na.rm = TRUE)
    if (max_vif >= vif_threshold)
        cat(sprintf("Multicollinearity detected: max VIF = %.2f (%s)\n", max_vif, cc$Term[which.max(cc$VIF)]))
    else
        cat(sprintf("No multicollinearity: max VIF = %.2f\n", max_vif))
}

check_poisson <- function(model, threshold = 1.5) {
    od <- sum(residuals(model, type = "pearson")^2) / model$df.residual
    degree  <- dplyr::case_when(od < 1.5 ~ "minimal", od < 3 ~ "mild", od < 10 ~ "moderate", TRUE ~ "severe")
    verdict <- if(od >= threshold) "NB model recommended" else "Poisson likely sufficient"
    cat(sprintf("Poisson overdispersion: %.2f (%s) — %s\n\n", od, degree, verdict))
}

# %%
my_cut <- function(my_value, range, my_min = 1) {
    if(is.na(my_value)){return("NA")}
    if(my_value == 0){return("0")}

    # Basic checks
    stopifnot(is.numeric(my_value),
              length(range) == 1, 
              is.numeric(range), 
              is.finite(range), 
              range > 0)

    if(my_value > 0 ){
        lower <- floor((my_value - 1) / range) * range + 1
        upper <- lower + range - 1
        if(!my_min == 1 & lower == 1){lower <- my_min}
        
        if(upper == lower){return(as.character(lower))}
        return(paste0(lower, " - ", upper))
    }
    if(my_value < 0){
        upper <- ceiling((my_value + 1) / range) * range - 1
        lower <- upper - range +1
        if(upper == lower){return(as.character(lower))}
        return(paste0(lower, " - ", upper))
    }
    
}

# %% [markdown]
# # Grab the data

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

# dte_cohort_data %>% head(10)

# %%
drug_matchedDS_info <- c("Nontreatment", "PS_Matched_Dataset-Nontreatment.rds",
                         "Insulins", "PS_Matched_Dataset-Insulins.rds",
                         "Metformin", "PS_Matched_Dataset-Metformin.rds",
                         "DPP4i", "PS_Matched_Dataset-DPP4i.rds",
                         "SGLT2i", "PS_Matched_Dataset-SGLT2i.rds",
                         "SU", "PS_Matched_Dataset-SU.rds",
                        #"TZD", "PS_Matched_Dataset-TZD.rds",
                         "GLP1RA", "PS_Matched_Dataset-GLP1RA.rds"
                        ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug", "file_path"))

# drug_matchedDS_info

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i])

    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")

    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext)))) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i])) %>%
    mutate(treatment_name = factor(treatment_name, levels = c(drug_matchedDS_info$drug[i], "Semaglutide")))


    var_name <- paste0("dte_cohort_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, matched.data)
}

# dte_cohort_Insulins %>% head

# %%
#                period                      bgn_win end_win   period_name
period_info <- c("6-0 months before index",     -180,      0,          "a",
                 "15 days-3 months after index",  15,     90,          "b",
                 "15 days-6 months after index",  15,    180,          "c",
                 "15 days-12 months after index", 15,    360,          "d",
                 "6-12 months after index",      180,    360,          "e"
                ) %>%
matrix(ncol = 4, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("period", "bgn_win", "end_win", "period_name")) %>%
mutate(across(c(bgn_win, end_win), ~as.numeric(.)))

period_info

# %%
load("all_outcomes.rds")

# all_outcomes %>% head(20)

# %%
all_outcomes_2 <- all_outcomes %>%
left_join(period_info %>% dplyr::select(period, period_name), by = "period") %>%
mutate(var_name = paste0(var_name, "_", period_name)) %>%
dplyr::select(-period, -period_name) %>%
pivot_wider(names_from = "var_name", values_from = "value") 

# all_outcomes_2 %>% head

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    this_cohort_name <- paste0("Semaglutide vs ", drug_matchedDS_info$drug[i])
    this_cohort <- get(paste0("dte_cohort_", drug_matchedDS_info$drug[i]))
    this_outcomes <- all_outcomes_2 %>%
    filter(study_cohort == this_cohort_name)
    
    this_cohort <- this_cohort %>%
    left_join(this_outcomes, by = join_by(person_id, study_cohort))
    
    var_name <- paste0("dte_cohort_and_outcomes_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, this_cohort)
}

# dte_cohort_and_outcomes_Insulins %>% head

# %%
# Load Nontreatment diagnosis timeline variables and join Obese and A1C columns
# into the Nontreatment cohort dataset.
load("data_DTE_DiagnosisTimelineVars_Nontreatment.rds")
nontreatment_diag_vars <- this.data %>%
    dplyr::select(person_id, Obese_Before_Nontreatment_Index, A1C_over_8p5_Before_Nontreatment_Index)

dte_cohort_and_outcomes_Nontreatment <- dte_cohort_and_outcomes_Nontreatment %>%
    left_join(nontreatment_diag_vars, by = "person_id")

# dte_cohort_and_outcomes_Nontreatment %>% head

# %%
# Load the consecutive treatment period table produced by
# get_Antidepressant_Treatment_Timeline_v2.R
load("antidepressant_antipsychotic_consecutive_period.rds")

# consecutive_period_tab %>% head

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i])

    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")

    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext)))) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i])) %>%
    mutate(treatment_name = factor(treatment_name, levels = c(drug_matchedDS_info$drug[i], "Semaglutide")))


    var_name <- paste0("dte_cohort_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, matched.data)
}

# dte_cohort_Insulins %>% head

# %%
# For each comparison population, filter consecutive_period_tab to
# participants in that matched cohort and to the 15-day–12-month follow-up window.
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug   <- drug_matchedDS_info$drug[i]
    this_cohort <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
        dplyr::select(person_id, treatment_name, first_drug_record) %>%
        mutate(treatment_name = factor(treatment_name, levels = c(this_drug, "Semaglutide")))

    this_period_tab <- consecutive_period_tab %>%
        inner_join(this_cohort, by = "person_id") %>%
        mutate(
            time_from_index_days = as.numeric(difftime(first_record, as.Date(first_drug_record), units = "days"))
        ) %>%
        filter(time_from_index_days >= 15 & time_from_index_days <= 360)

    var_name <- paste0("consecutive_period_", this_drug)
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, this_period_tab)
}

# consecutive_period_Insulins %>% head

# %% [markdown]
# # Sensitivity Analyses

# %% [markdown]
# ## Difference before and after antidiabetic index
#
# Goal: Compare 6-months prior to the 12-months post
#
# Method: use period as a covariate and as an interaction term in the NB model outcome ~ treatment + period + (treatment *period) + other covariates.
#
# Interpretation: the interaction term is the Difference-in-Differences (DID) estimand. If Semaglutide were already on a trajectory of frequent med changes, the treatment effect will shrink.
#

# %%
varsToFactor <- c("n_med_changes_a", "n_med_changes_d")
new_titles <- c("", "", "")
new_names <- c("n", "Total med changes 0-6 months before index (mean (SD))", "Total med changes 15 days-12 months after index (mean (SD))")

for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))
    
    my_table1(this.data = this_dataset, 
              my_strata = "treatment", 
              filename = paste0("Semaglutide_vs_", this_drug, "-Med_Changes"), 
              varsToFactor = varsToFactor,   
              new_colnames = c("Variable", "", paste0("TOTAL Semaglutide vs ", this_drug, " Population"), paste0(this_drug, " Population"), "Semaglutide Population", "p"),
              verbose = FALSE,
              new_titles = new_titles,
              new_names = new_names
              )
    }

# %%
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    this_dataset <- this_dataset %>% 
    dplyr::select(person_id, treatment, treatment_name, race.ethnicity_White, sex_Male, age_at_index_years, n_med_changes_a, n_med_changes_d) %>% 
    pivot_longer(cols = c("n_med_changes_a", "n_med_changes_d"), names_to = "period_name", values_to = "n_med_changes", values_drop_na = FALSE) %>%
    rowwise() %>%
    mutate(period_name = str_split(period_name, "_")[[1]][4]) %>%
    ungroup() %>%
    left_join(period_info, by = "period_name") %>%
    mutate(period_length = end_win - bgn_win,
           n_med_changes_per_day = n_med_changes / period_length) %>%
    mutate(period = factor(period, levels = c("6-0 months before index", "15 days-12 months after index"), ordered = TRUE))
    
    means_df <-  this_dataset %>%
  group_by(treatment_name, period) %>%
  summarise(
    n = sum(!is.na(n_med_changes)),
    mean_n = mean(n_med_changes, na.rm = TRUE),
    se_n = sd(n_med_changes, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    x = as.numeric(period),
    ymin = mean_n - se_n,
    ymax = mean_n + se_n
  )

    p <- ggplot(this_dataset, aes(x = period, y = n_med_changes, fill = treatment_name)) +
  geom_violin(trim = FALSE, alpha = 0.35,
              position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = treatment_name),
              position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
              alpha = 0.20, size = 1) +
  # mean line + mean points
  geom_line(data = means_df,
            aes(x = period, y = mean_n, group = treatment_name, color = treatment_name),
            linewidth = 1.4, inherit.aes = FALSE) +
  geom_point(data = means_df,
             aes(x = period, y = mean_n, color = treatment_name),
             size = 4, inherit.aes = FALSE) +
  labs(x = NULL, y = "n_med_changes", fill = "Treatment", color = "Treatment") +
  theme_minimal(base_size = 40) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

    print(p)
}

# %%
#theme_set(theme_sjplot())
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    this_dataset <- this_dataset %>% 
    dplyr::select(person_id, treatment, treatment_name, race.ethnicity_White, sex_Male, age_at_index_years, n_med_changes_a, n_med_changes_d) %>% 
    pivot_longer(cols = c("n_med_changes_a", "n_med_changes_d"), names_to = "period_name", values_to = "n_med_changes", values_drop_na = FALSE) %>%
    rowwise() %>%
    mutate(period_name = str_split(period_name, "_")[[1]][4]) %>%
    ungroup() %>%
    left_join(period_info, by = "period_name") %>%
    mutate(period_length = end_win - bgn_win,
           n_med_changes_per_day = n_med_changes / period_length) %>% 
    mutate(period = factor(period, levels = c("6-0 months before index", "15 days-12 months after index")))

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m_pois <- glm(
        n_med_changes ~ treatment_name + (treatment_name*period) + race.ethnicity_White + sex_Male + age_at_index_years + offset(log(period_length)),
        data = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(n_med_changes ~ treatment_name + (treatment_name*period) + race.ethnicity_White + sex_Male + age_at_index_years + offset(log(period_length)),
                 data = this_dataset)

    "*Negative Binomial*\n\n" %>% cat

    check_od(m1)
    check_mc(m1)
    
    # summary(m1) %>% print
    
    "\n\n" %>% cat
    
    "treatment_nameSemaglutide is the treatment effect in the 0-6 months before index\n\n" %>% str_glue %>% cat

    "period15 days-12 months after index is the period effect among the {this_drug} group.\n\n" %>% str_glue %>% cat

    "treatment_nameSemaglutide:period15 days-12 months after index tells you how much the treatment effect changes in 15 days-12 months after index vs 0-6 months before index.\n\n" %>% cat



    "\nsummary:\n\n" %>% cat

    tab <- tab_model(m1)
    display_html(tab$knitr)

    # IRR of treatment within each period
    trt_by_period <- emmeans(
        m1,
        ~ treatment_name | period,
        type = "response"          # gives rates on response scale (includes offset at its reference)
    )

    # Treatment contrasts within each period (IRR, CI, p)
    trt_effect_each_period <- contrast(
        trt_by_period,
        method = "pairwise",       # if 2 levels; use "revpairwise" to flip direction
        by = "period",
        type = "response"          # exponentiates: ratio of rates (IRR)
    )

    # "\n\n" %>% cat
    # trt_by_period %>% print
    "\n\n" %>% cat
    summary(trt_effect_each_period, infer = c(TRUE, TRUE)) %>% print

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# ## Ascertainment bias
#
# Goal: Demonstrate the relationship between number of med changes and total visits.
#
# Method: Perform an offset model by adding a term log(total_visits) to the NB model. Compare the the Internal Rate of Return (IRR) – i.e. the exp of the treatment coefficient – before and after adding the offset term.
#
# Interpretation: if IRR moves closer to 1 after normalizing by number of visits, this implies that the treatment effect is driven by engagement with the healthcare system.


# %%
#theme_set(theme_sjplot())
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m_pois <- glm(
        n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years + offset(log(n_visit_days_d+1)),
        data = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years + offset(log(n_visit_days_d+1)),
                 data = this_dataset)

    "*Negative Binomial*\n\n" %>% cat
    check_od(m1)
    check_mc(m1)
    # "\n\nresult:\n" %>% cat
    # summary(m1) %>% print
    
    "\nsummary:\n\n" %>% cat
    tab <- tab_model(m1)
    display_html(tab$knitr)

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# Among people with the same number of visit days, who had more medication changes?
#
# 1. Semaglutide vs metformin, DPP4i, and nontreatment largely disappears after accounting for visit days
#
# Metformin: 1.20 → 0.98
#
# DPP4i: 1.20 → 0.98
#
# Nontreatment: 1.29 → 1.01
#
# Interpretation:
#
# The higher raw total number of med changes in semaglutide users versus these groups appears to be explained largely by more visit exposure / more opportunities for a medication change to be recorded.
#
# After standardizing for visit days, semaglutide users do not appear to have a higher medication-change rate than these groups.
#
# This is a strong sign that healthcare utilization is an important driver of the original association for these comparators.
#
# 2. Semaglutide remains elevated versus SGLT2i, SU, and non-semaglutide GLP-1RA even after the offset
#
# SGLT2i: 1.26 → 1.25
#
# SU: 1.42 → 1.25
#
# Non-semaglutide GLP1RA: 1.30 → 1.31
#
# Interpretation:
#
# These differences are not explained just by more visit days.
#
# Even conditional on visit-day exposure, semaglutide users still had about:
#
# 25% higher med-change rate than SGLT2i users
#
# 25% higher than SU users
#
# 31% higher than non-semaglutide GLP-1RA users
#
# 3. Semaglutide vs insulins gets much stronger after the offset
#
# 1.13 (0.98–1.29) → 1.52 (1.29–1.79)
#
# This is the most informative shift.
#
# Interpretation:
#
# Before accounting for visit days, semaglutide and insulin users had fairly similar total counts, with only weak evidence of a difference.
#
# After accounting for visit-day exposure, semaglutide users had a 52% higher medication-change rate per visit day.
#
# That usually implies that the insulin group likely had more visit days, which created more opportunities for medication changes in absolute terms. Once you standardize for that, semaglutide users appear to have more changes relative to their amount of visit contact.
#
# So compared with insulin users:
#
# insulin users may have had higher utilization, but
#
# semaglutide users had more med changes per unit of utilization.

# %% [markdown]
# ## Hurdle model
#
# Goal: Determine whether treatment is associated with (1) binary outcome of either 0 or >0 medications and (2) among those with at least one change, is treatment associated with more frequent changes.
#
# Method: Run 2 separate models.
#
# - Logistic model predicting binary outcome of either 0 or >0 medication changes.
# - NB model that is the same as the original, but only includes participants with >0 medication changes.
#
# Interpretation: If treatment is significant for the logistic regression, that implies that treatment is associated with any med change. If treatment is significant for the restricted NB, that means that treatment is associated with a difference in med change frequency.
#
# Difficulty: Easy. Est 1-2 days.

# %%
#theme_set(theme_sjplot())
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
    mutate(has_med_change_d = n_med_changes_d > 0)

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat
    
    m1 <- glm(has_med_change_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
              data = this_dataset,
              family = binomial)

    "*Logistic Regression*\n\n" %>% cat
    paste0("\nhas_med_change_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years", "\n") %>% cat
    # check_collinearity(m1) %>% print
    # "\n\nresult:\n" %>% cat
    # summary(m1) %>% print

    "\nsummary:\n\n" %>% cat
    tab <- tab_model(m1)
    display_html(tab$knitr)

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %%
#theme_set(theme_sjplot())
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
    filter(n_med_changes_d > 0)

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat
    
    this_dataset %>% count(treatment_name) %>% print
    paste0("\n") %>% cat
    
    m_pois <- glm(
        n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
        data = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
                 data = this_dataset)

    "*Negative Binomial*\n\n" %>% cat
    paste0("\nn_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years", "\n") %>% cat
    check_od(m1)
    check_mc(m1)
    "\n\nresult:\n" %>% cat
    # summary(m1) %>% print
    
    "\nsummary:\n\n" %>% cat
    tab <- tab_model(m1)
    display_html(tab$knitr)

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# ## Index-year dependent sensitivity analysis
#
# Goal: Determine if treatment effect is consistent over time.
#
# Method: Categorize participants by index year. Perform a separate sensitivity analysis for each “common era”.
#
# Interpretation:
#
# Concerns: Site is hidden in AoU, therefore adding a “site x quarter” interaction term is impossible. Using quarter instead of year at index will create sparse categories due to limited sample size. Will need practitioner expertise to determine a “common era”.
#
# Difficulty: Easy. Est 1-2 days.

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))
    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat
    
    this_dataset %>% 
    count(first_drug_record_year, treatment_name) %>% 
    pivot_wider(names_from = treatment_name, values_from = n, values_fill = 0) %>%
    mutate(ratio := Semaglutide / !!sym(this_drug),
           total := Semaglutide + !!sym(this_drug)) %>%
    mutate(ratio := ifelse(!!sym(this_drug) < 20 | Semaglutide < 20, NA, ratio), # must hide values under 20
           total := ifelse(!!sym(this_drug) < 20 | Semaglutide < 20, NA, total),
           !!sym(this_drug) := ifelse(!!sym(this_drug) < 20, "<20", !!sym(this_drug)),
           Semaglutide = ifelse(Semaglutide < 20, "<20", Semaglutide)
          ) %>%
    print(n = Inf)
}

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    # paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    this_year_dist <- this_dataset %>%
    count(first_drug_record_year, treatment_name) %>%
    pivot_wider(names_from = treatment_name, values_from = n) %>%
    mutate(total := Semaglutide + !!sym(this_drug),
           ratio := Semaglutide / !!sym(this_drug)) %>%
    filter(!is.na(ratio) & ratio > 0.15 & !is.na(total) & total >= 100)

    results <- lapply(1:nrow(this_year_dist), function(j){
        this_year <- this_year_dist$first_drug_record_year[j]

        this_dataset_year_filtered <- this_dataset %>%
        filter(first_drug_record_year == this_year)

        m1 <- tryCatch(
            glm.nb(n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
                   data = this_dataset_year_filtered),
            error = function(e) NULL
        )

        if(is.null(m1)) return(data.frame(year = this_year, RR = NA, CI_lo = NA, CI_hi = NA, p_value = NA))

        coefs <- summary(m1)$coefficients
        term  <- "treatment_nameSemaglutide"
        est   <- coefs[term, "Estimate"]
        se    <- coefs[term, "Std. Error"]
        z     <- qnorm(0.975)

        data.frame(
            year    = this_year,
            RR      = exp(est),
            CI_lo   = exp(est - z * se),
            CI_hi   = exp(est + z * se),
            p_value = coefs[term, "Pr(>|z|)"]
        )
    }) %>% bind_rows()

    results %>%
    kbl(format = "html", digits = 3,
        col.names = c("Index Year", "Rate Ratio", "95% CI Lower", "95% CI Upper", "p-value"),
        caption = paste0("Point estimates by index year: Semaglutide vs ", this_drug)) %>%
    as.character %>%
    display_html
}

# %% [markdown]
# ## Account for matching
#
# Goal: Account for matching bias in the negative binomial regression.
#
# Method: After fitting the model, compute cluster-robust standard error by the matched set.
#
# Interpretation: If the SEs/CIs/p-values change, that means the significance of the estimate is dependent on the matching.
#
# Difficulty: Easy. Est 2-3 days.

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m1 <- glm.nb(n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
                 data = this_dataset)

    # Cluster-robust variance by matched set (pair/subclass)
    V_cl <- vcovCL(m1, cluster = this_dataset$subclass, type = "HC1")

    # Robust inference table (z tests)
    coeftest(m1, vcov. = V_cl)

    # IRRs + robust 95% CIs
    b  <- coef(m1)
    se <- sqrt(diag(V_cl))
    IRR <- exp(b)
    CI  <- exp(cbind(lo = b - 1.96*se, hi = b + 1.96*se))
    cbind(IRR, CI) %>% print

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# Accounting for matching leaves the CIs unchanged, indicating there is not a matching bias.

# %% [markdown]
# ## Identify antidiabetic discontinuation
#
# Goal: Determine if med changes are related to discontinuation of the antidiabetic
#
# Method:
#
# - Define continued use as > 0 records of the antidiabetic in the follow up period
# - Fit an as-treated outcome model weighted by the inverse probability of censoring weights (IPCW)
#
# Interpretation: The weighted as-treated IRR is the estimated rate ratio while on treatment.
#
# Concern: Very difficult to determine time of discontinuation from the EHR. Would probably be more manageable to define continued use as a binary (either <2 or >=2 records of the antidiabetic) and run a sensitivity analysis on both groups.
#
# Difficulty: Easy. Est 1-2 days.
#

# %%
drug_timeline_info <- c("Semaglutide", "Data_Prepped/Prepped_Data-All_Participants-Semaglutide-Drug_Exposure.rds",
                        "Insulins", "Data_Prepped/Prepped_Data-All_Participants-Insulins-Drug_Exposure.rds",
                        "Metformin", "Data_Prepped/Prepped_Data-All_Participants-Metformin-Drug_Exposure.rds",
                        "DPP4i", "Data_Prepped/Prepped_Data-All_Participants-DPP_4i-Drug_Exposure.rds",
                        "SGLT2i", "Data_Prepped/Prepped_Data-All_Participants-SGLT2i-Drug_Exposure.rds",
                        "SU", "Data_Prepped/Prepped_Data-All_Participants-SU-Drug_Exposure.rds",
                        "TZD", "Data_Prepped/Prepped_Data-All_Participants-TZD-Drug_Exposure.rds",
                        "GLP1RA", "Data_Prepped/Prepped_Data-All_Participants-GLP_1RAs_Excluding_Semaglutide-Drug_Exposure.rds"
                        ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug", "file_path"))

drug_timeline_info

# %%
# Loop over each file path
for(i in 1:nrow(drug_timeline_info)){
    paste0("Loading file \"", drug_timeline_info$file_path[i], "\"\n") %>% cat
    load(drug_timeline_info$file_path[i])
    
    var_name <- paste0(drug_timeline_info$drug[i], "_drug_exposure")
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, data_prep)
}

# %%
this_bgn_win <- 15
this_end_win <- 360

for(i in 2:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    this_semaglutide_exposure <- Semaglutide_drug_exposure %>%
    arrange(person_id, start_datetime) %>%
    filter(person_id %in% (this_dataset %>% filter(treatment_name == "Semaglutide") %>% pull(person_id))) %>%
    left_join(this_dataset %>% dplyr::select(person_id, treatment_name, first_drug_record), by = "person_id") %>%
    mutate(time_from_index_days = time_length(interval(first_drug_record, start_datetime), "days")) %>%
    filter(time_from_index_days >= this_bgn_win & time_from_index_days <= this_end_win) %>%
    group_by(person_id, start_datetime) %>%
    slice(1) %>%
    group_by(person_id) %>%
    summarize(n_follow_up_ad_records = n(), .groups = "drop") %>%
    ungroup()

    this_comparison_exposure <- get(paste0(this_drug, "_drug_exposure")) %>%
    arrange(person_id, start_datetime) %>%
    filter(person_id %in% (this_dataset %>% filter(treatment_name == this_drug) %>% pull(person_id))) %>%
    left_join(this_dataset %>% dplyr::select(person_id, treatment_name, first_drug_record), by = "person_id") %>%
    mutate(time_from_index_days = time_length(interval(first_drug_record, start_datetime), "days")) %>%
    filter(time_from_index_days >= this_bgn_win & time_from_index_days <= this_end_win) %>%
    group_by(person_id, start_datetime) %>%
    slice(1) %>%
    group_by(person_id) %>%
    summarize(n_follow_up_ad_records = n(), .groups = "drop") %>%
    ungroup()

    this_dataset <- this_dataset %>%
    left_join(rbind(this_semaglutide_exposure, this_comparison_exposure), by = "person_id") %>%
    mutate(n_follow_up_ad_records = ifelse(is.na(n_follow_up_ad_records), 0, n_follow_up_ad_records)) %>%
    mutate(continued_ad_use = n_follow_up_ad_records > 0) 
    n_before_filter <- nrow(this_dataset)
    
    this_dataset <- this_dataset %>%
    filter(continued_ad_use)

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n") %>% cat
    paste0("original cohort size: ", comma(n_before_filter),"\n\n") %>% cat

    m_pois <- glm(
        n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
        data = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(n_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
                 data = this_dataset)

    "*Negative Binomial*\n\n" %>% cat
    check_od(m1)
    check_mc(m1)
    # "\n\nresult:\n" %>% cat
    # summary(m1) %>% print

    "\nsummary:\n\n" %>% cat
    interpret_nb(m1, term = "treatment_nameSemaglutide", outcome = "total med changes", comparison = this_drug)

    tab <- tab_model(m1)
    display_html(tab$knitr)

    p <- plot_model(m1) +
    theme_minimal(base_size = 20) 

    #p %>% print()


    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# ## Test negative controls
#
# Goal: Demonstrate that the changes in medication are unique to antidepressants.
#
# Method: Use the following negative controls:
#
# - Changes in a medication class unrelated to mood but similarly well documented (e.g. antidieuretics)
# - Changes in medications pre-index
#
# Interpretation: If effect is strictly within antidepressants post-index, this supports the idea that there is a mood effect of the treatment.
#
# Difficulty: Medium. It will take a little extra time to define the additional drug groups. Est 3-4 days.
#
# GRF to est heterogeneity

# %%
# Negative control 1: Pre-index medication changes (temporal falsification test)
# If treatment assignment predicts pre-index med changes, this indicates unmeasured confounding.
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug    <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m_pois <- glm(
        n_med_changes_a ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
        data   = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(
        n_med_changes_a ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
        data = this_dataset
    )

    "*Negative Binomial*\n\n" %>% cat
    check_od(m1)
    check_mc(m1)
    "\nsummary:\n\n" %>% cat
    interpret_nb(m1, term = "treatment_nameSemaglutide",
                 outcome   = "pre-index med changes (negative control)",
                 comparison = this_drug)

    tab <- tab_model(m1)
    display_html(tab$knitr)

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %%
# Negative control 2: Unrelated drug class medication changes (post-index)
# Uses hydrochlorothiazide (HCTZ) med changes post-index as the outcome.
# HCTZ is unrelated to mood; a null effect here supports specificity of the
# antidepressant finding.
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug    <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m_pois <- glm(
        n_hc_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
        data   = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(
        n_hc_med_changes_d ~ treatment_name + race.ethnicity_White + sex_Male + age_at_index_years,
        data = this_dataset
    )

    "*Negative Binomial*\n\n" %>% cat
    check_od(m1)
    check_mc(m1)
    "\nsummary:\n\n" %>% cat
    interpret_nb(m1, term = "treatment_nameSemaglutide",
                 outcome    = "hydrochlorothiazide med changes (negative control)",
                 comparison = this_drug)

    tab <- tab_model(m1)
    display_html(tab$knitr)

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# ## Effect modification analysis
#
# Goal: See if the treatment effect is modified by other factors.
#
# Method: Add interaction terms to treatment:
# - Sex
# - BMI
# - A1c
# - Baseline psych utilization
# - Baseline antidepressant burden
#
# Interpretation: If the interaction is significant, that indicates the treatment effect is modified by other factors.
#
# Difficulty: Easy. Est 1-2 days.

# %%
# Print IRR + 95% CI + p-value for the treatment × modifier interaction term
summarize_interaction <- function(model, mod_label, trt_term = "treatment_nameSemaglutide",
                                  alpha = 0.05, digits = 2) {
    coefs    <- summary(model)$coefficients
    int_rows <- rownames(coefs)[grepl(paste0("^", trt_term, ":"), rownames(coefs))]
    if(length(int_rows) == 0){
        cat("  Interaction term not found.\n")
        return(invisible(NULL))
    }
    term <- int_rows[1]
    est  <- coefs[term, "Estimate"]
    se   <- coefs[term, "Std. Error"]
    z    <- qnorm(1 - alpha / 2)
    rr   <- exp(est); lo <- exp(est - z * se); hi <- exp(est + z * se)
    pval <- coefs[term, "Pr(>|z|)"]
    sig  <- if(pval < alpha) " *" else ""
    cat(sprintf("  Interaction (%s): IRR = %.2f (95%% CI %.2f-%.2f), p = %.4f%s\n",
                mod_label, rr, lo, hi, pval, sig))
}

# Print stratum-specific treatment IRRs from an emmeans contrast object
summarize_strata <- function(emm_contrast, mod_var, digits = 2) {
    cs <- summary(emm_contrast, infer = c(TRUE, TRUE))
    for(j in seq_len(nrow(cs))){
        cat(sprintf("    %s = %-5s  IRR = %.2f (95%% CI %.2f-%.2f), p = %.4f\n",
                    mod_var, as.character(cs[[mod_var]][j]),
                    cs$ratio[j], cs$asymp.LCL[j], cs$asymp.UCL[j], cs$p.value[j]))
    }
}

# %%
effect_modifiers <- list(
    list(var = "sex_Male",       label = "Sex (Male)",                                      binary = TRUE),
    list(var = "Obese_Before_drug_Index",       label = "Obesity",                           binary = TRUE),
    list(var = "A1C_over_8p5_Before_drug_Index", label = "A1C > 8.5%",                      binary = TRUE),
    list(var = "n_psych_visits_a",
         label = "Baseline psych utilization (visits/year)",         binary = FALSE),
    list(var = "n_med_changes_a", label = "Baseline antidepressant burden (pre-index med changes)", binary = FALSE)
)

base_covars <- c("race.ethnicity_White", "sex_Male", "age_at_index_years")

for(i in 1:nrow(drug_matchedDS_info)){
    this_drug    <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    # Create drug-specific Obese and A1C columns: pick the flag for the
    # participant's actual treatment arm (Semaglutide or the comparator drug).
    this_dataset <- this_dataset %>%
    mutate(
        Obese_Before_drug_Index = if_else(
            treatment_name == "Semaglutide",
            Obese_Before_Semaglutide_Index,
            !!sym(paste0("Obese_Before_", this_drug, "_Index"))
        ),
        A1C_over_8p5_Before_drug_Index = if_else(
            treatment_name == "Semaglutide",
            A1C_over_8p5_Before_Semaglutide_Index,
            !!sym(paste0("A1C_over_8p5_Before_", this_drug, "_Index"))
        )
    )

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n") %>% cat

    for(mod in effect_modifiers){
        mod_var    <- mod$var
        mod_label  <- mod$label
        mod_binary <- mod$binary

        paste0("\n*Effect modifier: ", mod_label, "*\n") %>% cat

        remaining_covars <- setdiff(base_covars, mod_var)
        formula_str <- paste0(
            "n_med_changes_d ~ treatment_name * ", mod_var, " + ",
            paste(remaining_covars, collapse = " + ")
        )

        m1 <- tryCatch(
            glm.nb(as.formula(formula_str), data = this_dataset),
            error = function(e){ cat("  Model failed:", conditionMessage(e), "\n"); NULL }
        )
        if(is.null(m1)) next

        check_od(m1)
        summarize_interaction(m1, mod_label)

        if(mod_binary){
            emm <- emmeans(m1, as.formula(paste0("~ treatment_name | ", mod_var)), type = "response")
            summarize_strata(contrast(emm, method = "pairwise", by = mod_var, type = "response"), mod_var)
        }
    }

    "--------------------------------------------------------------------------------------------" %>% cat
}

# %%
