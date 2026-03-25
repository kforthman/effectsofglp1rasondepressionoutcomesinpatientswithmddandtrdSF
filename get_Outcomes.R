# -*- coding: utf-8 -*-
# %% [markdown]
# # Setup

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %% [markdown]
# ## Load in and preview data

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

dte_cohort_data %>% head(10)

# %%
load("dte_cohort_visits.rds")

dte_cohort_visits <- visit_data

dte_cohort_visits %>% head(10)

# %%
load("dte_cohort_psych_visits.rds")

dte_cohort_psych_visits <- psych_visit_data

dte_cohort_psych_visits %>% head(10)

# %%
load("antidepressant_antipsychotic_consecutive_period.rds", verbose = TRUE)

dte_cohort_drug_use <- consecutive_period_tab

dte_cohort_drug_use %>% head

# %%
load("hydrochlorothiazide_consecutive_instance.rds", verbose = TRUE)

dte_cohort_hc_drug_use <- consecutive_instance_tab

dte_cohort_hc_drug_use %>% head

# %%
# Check that rows are unique
cat("\nTotal visit records:\n")
dte_cohort_visits %>% nrow %>% comma %>% cat
cat("\nTotal unique visit ids:\n")
dte_cohort_visits %>% pull(visit_occurrence_id) %>% unique %>% length %>% comma %>% cat
#cat("\nDuplicate rows:\n")
#dte_cohort_visits %>% group_by_all() %>% summarize(COUNT = n(), .groups = "keep") %>% filter(COUNT > 1) %>% ungroup()

# %% [markdown]
# ## Load in cohort definitions

# %%
drug_matchedDS_info <- c("Insulins", "PS_Matched_Dataset-Insulins.rds",
                        "Metformin", "PS_Matched_Dataset-Metformin.rds",
                        "DPP4i", "PS_Matched_Dataset-DPP4i.rds",
                        "SGLT2i", "PS_Matched_Dataset-SGLT2i.rds",
                        "SU", "PS_Matched_Dataset-SU.rds",
                        #"TZD", "PS_Matched_Dataset-TZD.rds",
                        "GLP1RA", "PS_Matched_Dataset-GLP1RA.rds",
                        "Nontreatment", "PS_Matched_Dataset-Nontreatment.rds"
                        ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug", "file_path"))

drug_matchedDS_info

# %% [markdown]
# ## Subset dataset for each cohort

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i]) # matched.data

    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")

    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide",
                                       Semaglutide_first_drug_record,
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext))))

    study_cohort_label <- paste0("Semaglutide vs ", drug_matchedDS_info$drug[i])
    matched_slim       <- matched.data %>% dplyr::select(person_id, treatment, treatment_name, first_drug_record)
    treatment_levels   <- c("Semaglutide", drug_matchedDS_info$drug[i])

    var_name <- paste0("dte_cohort_ids_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, matched.data %>%
        dplyr::select(person_id) %>%
        mutate(study_cohort = study_cohort_label))

    var_name <- paste0("dte_cohort_visits_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, dte_cohort_visits %>%
        filter(person_id %in% matched.data$person_id) %>%
        left_join(matched_slim, by = join_by(person_id == person_id)) %>%
        mutate(time_from_index_days   = as.numeric(difftime(start_datetime, first_drug_record, units = "days")),
               time_from_index_months = time_from_index_days / 30.4375,
               treatment_name         = factor(treatment_name, levels = treatment_levels),
               n_days                 = as.numeric(difftime(end_datetime, start_datetime, units = "days")) + 1,
               study_cohort           = study_cohort_label))

    var_name <- paste0("dte_cohort_psych_visits_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, dte_cohort_psych_visits %>%
        filter(person_id %in% matched.data$person_id) %>%
        left_join(matched_slim, by = join_by(person_id == person_id)) %>%
        mutate(time_from_index_days   = as.numeric(difftime(start_datetime, first_drug_record, units = "days")),
               time_from_index_months = time_from_index_days / 30.4375,
               treatment_name         = factor(treatment_name, levels = treatment_levels),
               study_cohort           = study_cohort_label))

    var_name <- paste0("dte_cohort_drug_use_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, dte_cohort_drug_use %>%
        filter(person_id %in% matched.data$person_id) %>%
        left_join(matched_slim, by = join_by(person_id == person_id)) %>%
        mutate(time_from_index_days   = as.numeric(difftime(first_record, first_drug_record, units = "days")),
               time_from_index_months = time_from_index_days / 30.4375,
               treatment_name         = factor(treatment_name, levels = treatment_levels),
               n_days                 = as.numeric(difftime(last_record, first_record, units = "days")) + 1,
               study_cohort           = study_cohort_label))

    var_name <- paste0("dte_cohort_hc_drug_use_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, dte_cohort_hc_drug_use %>%
        filter(person_id %in% matched.data$person_id) %>%
        left_join(matched_slim, by = join_by(person_id == person_id)) %>%
        mutate(time_from_index_days   = as.numeric(difftime(first_record, first_drug_record, units = "days")),
               time_from_index_months = time_from_index_days / 30.4375,
               treatment_name         = factor(treatment_name, levels = treatment_levels),
               n_days                 = as.numeric(difftime(last_record, first_record, units = "days")) + 1,
               study_cohort           = study_cohort_label))

    "\n" %>% cat
}

dte_cohort_ids_allSets <- rbind(
    dte_cohort_ids_Insulins,
    dte_cohort_ids_Metformin,
    dte_cohort_ids_DPP4i,
    dte_cohort_ids_SGLT2i,
    dte_cohort_ids_SU,
    #dte_cohort_ids_TZD,
    dte_cohort_ids_GLP1RA,
    dte_cohort_ids_Nontreatment
)

dte_cohort_ids_allSets <- dte_cohort_ids_allSets %>% mutate(study_cohort = as.factor(study_cohort))

dte_cohort_ids_allSets %>% head(10)

dte_cohort_ids_allSets %>% dim

dte_cohort_ids_allSets$study_cohort %>% table()


# %% [markdown]
# ## Define periods of interest

# %%
#                period                      bgn_win end_win
period_info <- c("6-0 months before index",     -180,      0,
                 "15 days-3 months after index",  15,     90,
                 "15 days-6 months after index",  15,    180,
                 "15 days-12 months after index", 15,    360,
                 "6-12 months after index",      180,    360
                ) %>%
matrix(ncol = 3, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("period", "bgn_win", "end_win")) %>%
mutate(across(c(bgn_win, end_win), ~as.numeric(.)))

period_info

# %% [markdown]
# ## Custom functions

# %%
my_histogram <- function(data, var, title, subtitle = "", xlab, ylab, ylog = F, xscalefactor = 1, labscalefactor = 1){
    hist_data <- data.frame(hist_var = data %>% pull(!!sym(var)))

    n_val <- max(hist_data$hist_var)
    int_val <- 10^(nchar(as.character(n_val)) - 2)*xscalefactor
    max_val <- ceiling(n_val/int_val)*int_val

    if(ylog){
        ggplot(hist_data, aes(hist_var)) +
        geom_histogram(aes(y = after_stat(count + 1)),
                       breaks = seq(0, max_val, by = int_val),
                       fill = "steelblue",
                       color = "white") +
        geom_text(
            stat    = "bin",
            breaks  = seq(0, max_val, by = int_val),
            aes(
                label = comma(after_stat(count)), 
                y     = after_stat((count + 1) * 1.05)     # 1.05× puts it just above
            ),
            size = 7*labscalefactor,
            vjust = 0, hjust = 0.25
        ) +
        scale_x_continuous(
            breaks = seq(0, max_val, by = int_val),
            expand = c(0.01, 0)
        ) +
        scale_y_log10() +
        annotation_logticks(sides = "l") +
        labs(
            title    = title,
            subtitle = subtitle,
            x        = xlab,
            y        = ylab,
        )  +
        theme_minimal(base_size = 25*labscalefactor) 
    }else{
        ggplot(hist_data, aes(hist_var)) +
        geom_histogram(breaks = seq(0, max_val, by = int_val),
                       fill = "steelblue",
                       color = "white") +
        geom_text(
            stat    = "bin",
            breaks  = seq(0, max_val, by = int_val),
            aes(
                label = comma(after_stat(count)), 
                y     = after_stat(count)     # 1.05× puts it just above
            ),
            size = 7*labscalefactor,
            vjust = 0, hjust = 0.25
        ) +
        scale_x_continuous(
            breaks = seq(0, max_val, by = int_val),
            expand = c(0.01, 0)
        ) +
        labs(
            title    = title,
            subtitle = subtitle,
            x        = xlab,
            y        = ylab,
        )  +
        theme_minimal(base_size = 25*labscalefactor)
    }
}

# %%
get_total_visits <- function(this_visit_data, all_participant_ids, out_var_name){
    out <- this_visit_data %>%
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_visit_days <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>%
    uncount(n_days) %>% # Create a row for each day of each inpatient visit
    group_by(person_id, visit_occurrence_id, study_cohort) %>%
    mutate(
        visit_date = start_datetime + days(row_number() - 1)
    ) %>%
    mutate(time_to_visit = interval(first_drug_record, visit_date) / days(1)) %>%
    filter(time_to_visit>=bgn_win & time_to_visit <= end_win) %>% # Exclude days of visit that are beyond x time after index
    group_by(person_id, visit_date, study_cohort) %>%
    slice_head(n = 1) %>% # Collapse visits by date
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_outpatient_visits <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Outpatient Visit") %>% 
    filter(n_days == 1) %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_outpatient_days <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Outpatient Visit") %>% 
    filter(n_days == 1) %>% 
    group_by(person_id, study_cohort, start_datetime) %>%
    slice_head(n = 1) %>%
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_first_outpatient <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Outpatient Visit") %>% 
    filter(n_days == 1) %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_second_outpatient <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Outpatient Visit") %>% 
    filter(n_days == 1) %>% 
    arrange(study_cohort, person_id, time_from_index_days)%>%
    group_by(person_id, study_cohort) %>%
    slice_tail(n = -1) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_inpatient_visits <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Inpatient Visit") %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_inpatient_days <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Inpatient Visit") %>%
    uncount(n_days) %>% # Create a row for each day of each inpatient visit
    group_by(person_id, visit_occurrence_id, study_cohort) %>%
    mutate(
        visit_date = start_datetime + days(row_number() - 1)
    ) %>%
    mutate(time_to_visit = interval(first_drug_record, visit_date) / days(1)) %>%
    filter(time_to_visit>=bgn_win & time_to_visit <= end_win) %>% # Exclude days of visit that are beyond x time after index
    group_by(person_id, visit_date, study_cohort) %>%
    slice_head(n = 1) %>% # Collapse visits by date
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_first_inpatient <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Inpatient Visit") %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_second_inpatient <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(standard_concept_name == "Inpatient Visit") %>% 
    arrange(study_cohort, person_id, time_from_index_days)%>%
    group_by(person_id, study_cohort) %>%
    slice_tail(n = -1) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_psych_visits <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_psych_days <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    group_by(person_id, study_cohort, start_datetime) %>%
    slice_head(n = 1) %>%
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_first_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    arrange(study_cohort, person_id, time_from_index_days)%>%
    group_by(person_id, study_cohort, time_from_index_days) %>%
    slice_head(n = 1) %>%
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_second_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    arrange(study_cohort, person_id, time_from_index_days)%>%
    group_by(person_id, study_cohort, time_from_index_days) %>%
    slice_head(n = 1) %>%
    group_by(person_id, study_cohort) %>%
    slice_tail(n = -1) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_third_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    arrange(study_cohort, person_id, time_from_index_days) %>%
    group_by(person_id, study_cohort, time_from_index_days) %>%
    slice_head(n = 1) %>%
    group_by(person_id, study_cohort) %>%
    slice_tail(n = -2) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_first_crisis_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(source_concept_code %in% c("90839", "90840")) %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = min(time_from_index_days), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_cum_acuity <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = sum(acuity), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) #%>%
    #mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_cum_acuity_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = sum(level), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) #%>%
    #mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_max_acuity_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    group_by(person_id, study_cohort) %>%
    summarize(outcome_var = max(level), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) #%>%
    #mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_n_levelx_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win, x){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    group_by(person_id, study_cohort) %>%
    filter(level == x) %>%
    summarize(outcome_var = n(), .groups = "keep") %>%
    ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) #%>%
    #mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_time_to_ith_levelx_psych <- function(this_visit_data, all_participant_ids, out_var_name, bgn_win, end_win, i, x){
    out <- this_visit_data %>% 
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    filter(level == x) %>%
    group_by(person_id, study_cohort, time_from_index_days) %>%
    slice(1) %>%
    group_by(person_id, study_cohort) %>%
    mutate(event_order = row_number()) %>%
    ungroup() %>%
    filter(event_order == i) %>%
    mutate(outcome_var = time_from_index_days) %>%
    ungroup() %>%
    dplyr::select(person_id, study_cohort, outcome_var)

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort))
    
    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
}

get_n_med_changes <- function(this_med_data, all_participant_ids, out_var_name){
    out <- this_med_data %>%
        group_by(person_id, study_cohort) %>%
        summarize(outcome_var = n(), .groups = "keep") %>%
        ungroup()

    out <- all_participant_ids %>%
    left_join(out, by = join_by(person_id, study_cohort)) %>%
    mutate(outcome_var = ifelse(is.na(outcome_var), 0, outcome_var))

    out <- out %>%
    rename(!!sym(out_var_name) := "outcome_var")

    return(out)
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
# ## Compute outcomes

# %%
# Flatten drug × period into independent tasks and pre-filter each to its
# window in the main process. Workers receive only the small, already-filtered
# slice they need — no redundant data transfer.
drug_list <- drug_matchedDS_info$drug[drug_matchedDS_info$drug != "Semaglutide"]

n_tasks   <- length(drug_list) * nrow(period_info)
n_drugs   <- length(drug_list)
task_list <- vector("list", n_tasks)
k <- 0L
flush.console(); cat(sprintf("Building %d tasks (%d drugs × %d periods)...\n",
                              n_tasks, n_drugs, nrow(period_info)))
for (drug in drug_list) {
    ds <- list(
        visits   = get(paste0("dte_cohort_visits_",       drug)),
        psych    = get(paste0("dte_cohort_psych_visits_", drug)),
        drugs    = get(paste0("dte_cohort_drug_use_",     drug)),
        hc_drugs = get(paste0("dte_cohort_hc_drug_use_",  drug)),
        ids      = get(paste0("dte_cohort_ids_",          drug))
    )
    for (j in seq_len(nrow(period_info))) {
        bgn <- period_info$bgn_win[j]
        end <- period_info$end_win[j]
        k   <- k + 1L
        task_list[[k]] <- list(
            this_period    = period_info$period[j],
            this_bgn_win   = bgn,
            this_end_win   = end,
            these_ids      = ds$ids,
            visits_w       = ds$visits   %>% filter(time_from_index_days >= bgn & time_from_index_days < end),
            psych_visits_w = ds$psych    %>% filter(time_from_index_days >= bgn & time_from_index_days < end),
            drugs_w        = ds$drugs    %>% filter(time_from_index_days >= bgn & time_from_index_days < end),
            hc_drugs_w     = ds$hc_drugs %>% filter(time_from_index_days >= bgn & time_from_index_days < end)
        )
    }
    flush.console(); cat(sprintf("  [%d/%d] Filtered data for: %s\n", which(drug_list == drug), n_drugs, drug))
}

# Cap workers at the number of tasks; no benefit beyond that.
n_workers <- min(max(1L, detectCores(logical = TRUE) - 1L), length(task_list))
cl <- makeCluster(n_workers)
registerDoParallel(cl)
flush.console(); cat(sprintf("\nStarting parallel computation: %d tasks on %d workers...\n",
                              length(task_list), getDoParWorkers()))

all_outcomes <- foreach(
    task      = task_list,
    .combine  = bind_rows,
    .packages = c("dplyr", "tidyr", "lubridate"),
    .export   = c("get_total_visits", "get_visit_days", "get_n_med_changes")
) %dopar% {

    these_ids      <- task$these_ids
    visits_w       <- task$visits_w
    psych_visits_w <- task$psych_visits_w
    drugs_w        <- task$drugs_w
    hc_drugs_w     <- task$hc_drugs_w
    this_bgn_win   <- task$this_bgn_win
    this_end_win   <- task$this_end_win

    this_data        <- these_ids
    this_data$period <- task$this_period

    this_data$n_visits         <- get_total_visits(visits_w,    these_ids, "x") %>% pull(x)
    this_data$n_visit_days     <- get_visit_days(visits_w,      these_ids, "x", this_bgn_win, this_end_win) %>% pull(x)
    this_data$n_med_changes    <- get_n_med_changes(drugs_w,    these_ids, "x") %>% pull(x)
    this_data$n_hc_med_changes <- get_n_med_changes(hc_drugs_w, these_ids, "x") %>% pull(x)

    # Pass 1 – aggregate counts and acuity (one group_by, no sorting needed)
    psych_agg <- psych_visits_w %>%
    group_by(person_id, study_cohort) %>%
    summarize(
        n_psych_visits   = n(),
        n_psych_days     = n_distinct(start_datetime),
        cum_acuity_psych = sum(level),
        max_acuity_psych = max(level),
        n_level1_psych   = sum(level == 1L),
        n_level2_psych   = sum(level == 2L),
        n_level3_psych   = sum(level == 3L),
        n_level4_psych   = sum(level == 4L),
        n_level5_psych   = sum(level == 5L),
        .groups = "drop"
    )

    # Pass 2 – time to 1st/2nd/3rd unique psych visit day
    psych_nth <- psych_visits_w %>%
    distinct(person_id, study_cohort, time_from_index_days) %>%
    arrange(person_id, study_cohort, time_from_index_days) %>%
    group_by(person_id, study_cohort) %>%
    summarize(
        time_to_first_psych  = time_from_index_days[1L],
        time_to_second_psych = time_from_index_days[2L],
        time_to_third_psych  = time_from_index_days[3L],
        .groups = "drop"
    )

    # Pass 3 – time to 1st/2nd/3rd unique level-2 psych visit day
    psych_level2_nth <- psych_visits_w %>%
    filter(level == 2L) %>%
    distinct(person_id, study_cohort, time_from_index_days) %>%
    arrange(person_id, study_cohort, time_from_index_days) %>%
    group_by(person_id, study_cohort) %>%
    summarize(
        time_to_1st_level2_psych = time_from_index_days[1L],
        time_to_2nd_level2_psych = time_from_index_days[2L],
        time_to_3rd_level2_psych = time_from_index_days[3L],
        .groups = "drop"
    )

    psych_all <- these_ids %>%
    left_join(psych_agg,        by = join_by(person_id, study_cohort)) %>%
    left_join(psych_nth,        by = join_by(person_id, study_cohort)) %>%
    left_join(psych_level2_nth, by = join_by(person_id, study_cohort)) %>%
    mutate(n_psych_visits = replace_na(n_psych_visits, 0L),
           n_psych_days   = replace_na(n_psych_days,   0L))

    this_data$n_psych_visits           <- psych_all$n_psych_visits
    this_data$n_psych_days             <- psych_all$n_psych_days
    this_data$time_to_first_psych      <- psych_all$time_to_first_psych
    this_data$time_to_second_psych     <- psych_all$time_to_second_psych
    this_data$time_to_third_psych      <- psych_all$time_to_third_psych
    this_data$cum_acuity_psych         <- psych_all$cum_acuity_psych
    this_data$max_acuity_psych         <- psych_all$max_acuity_psych
    this_data$n_level1_psych           <- psych_all$n_level1_psych
    this_data$n_level2_psych           <- psych_all$n_level2_psych
    this_data$n_level3_psych           <- psych_all$n_level3_psych
    this_data$n_level4_psych           <- psych_all$n_level4_psych
    this_data$n_level5_psych           <- psych_all$n_level5_psych
    this_data$time_to_1st_level2_psych <- psych_all$time_to_1st_level2_psych
    this_data$time_to_2nd_level2_psych <- psych_all$time_to_2nd_level2_psych
    this_data$time_to_3rd_level2_psych <- psych_all$time_to_3rd_level2_psych

    this_data %>%
    pivot_longer(4:ncol(this_data), names_to = "var_name", values_to = "value")
}

flush.console(); cat(sprintf("All %d tasks complete.\n\n", length(task_list)))
stopCluster(cl)

all_outcomes <- all_outcomes %>%
mutate(period = factor(period, levels = c(
    "6-0 months before index",
    "15 days-3 months after index",
    "15 days-6 months after index",
    "15 days-12 months after index",
    "6-12 months after index"))) %>%
mutate(study_cohort = factor(study_cohort, c(
    "Semaglutide vs Insulins", 
    "Semaglutide vs Metformin",
    "Semaglutide vs DPP4i", 
    "Semaglutide vs SGLT2i",
    "Semaglutide vs SU", 
    #  "Semaglutide vs TZD", 
    "Semaglutide vs GLP1RA",
    "Semaglutide vs Nontreatment"
))) %>%
mutate(var_name = factor(var_name))

# %%
save(all_outcomes, file = "all_outcomes.rds")

# %% [markdown]
# # Results

# %% [markdown]
# ## Table preview

# %%
all_outcomes %>% head(20)

# %% [markdown]
# ## Outcomes summaries

# %%
all_outcomes %>% 
group_by(study_cohort, period, var_name) %>%
summarize(
    n = n(), 
    n_0 = sum(value == 0, na.rm = T), 
    n_NA = sum(is.na(value)), 
    missingness = (n_0 + n_NA)/n,
    avg = mean(value, na.rm = T), 
    sd = sd(value, na.rm = T),
    se = sd/sqrt(n),
    min = ifelse(missingness == 1, NA, min(value, na.rm = T)),
    max = ifelse(missingness == 1, NA, max(value, na.rm = T)),
    .groups = "keep") %>%
mutate(
    across(c(n, n_0, n_NA), ~comma(.)),
    across(c(avg, sd, se), ~round(., 2)),
    across(c(missingness), ~percent(.))
      ) %>%
arrange(study_cohort, var_name, period) %>%
filter(period == "15 days-12 months after index") %>%
print_all_rows

# %% [markdown]
# ## Outcomes barplots

# %%
options(repr.plot.width=37, repr.plot.height=17)
plot_list <- list()
n_color <- 1

for(this_var_name in levels(all_outcomes$var_name)){
    my_min <- 1
    if(str_starts(this_var_name, "time_to", negate = FALSE)){
        my_range <- 30
        my_min <- 15
    }else if(str_starts(this_var_name, "max_acuity", negate = FALSE)){
        my_range <- 1
    }else if(str_starts(this_var_name, "n_", negate = FALSE)){
        my_range <- 10
    }else if(str_starts(this_var_name, "cum_", negate = FALSE)){
        my_range <- 15}
        
    for(this_period in levels(all_outcomes$period)){

        this_color <- c("#60408F", "#F68826", "#ED2D87", "#FFC920", "#97D8EB")[n_color] #, "#56C596"
        n_color <- ifelse(n_color + 1 > 5, 1, n_color+1)
        
        if(str_starts(this_var_name, "time_to", negate = FALSE) & this_period == "6-0 months before index"){next}
        if(str_starts(this_var_name, "time_to", negate = FALSE) & this_period == "6-12 months after index"){next}
    
        for(this_cohort in levels(all_outcomes$study_cohort)){

            this_df <- all_outcomes %>%
            filter(var_name == this_var_name) %>%
            filter(study_cohort == this_cohort) %>%
            filter(period == this_period) 
            
            n0 <- this_df %>% filter(value == 0) %>% nrow()
            nNA <- this_df %>% filter(is.na(value)) %>% nrow()
            nt <- this_df %>% nrow()
            

            these_levels <- this_df %>%
            rowwise %>%
            mutate(value2 = my_cut(value, my_range, my_min)) %>%
            ungroup() %>%
            arrange(value) %>% 
            pull(value2) %>% 
            unique

            this_df <- this_df %>%
            rowwise %>%
            mutate(value2 = my_cut(value, my_range, my_min)) %>%
            ungroup() %>%
            mutate(value3 = factor(value2, levels = these_levels))

            this_df <- this_df %>% count(value3) %>% rename(value = "value3")
            
            y_max <- max(this_df$n, na.rm = T)
            y_max <- y_max*1.1

            p <- ggplot(this_df, aes(x = value, y = n)) +
            geom_bar(stat = "identity", fill = this_color, color = "white")+
            geom_text(aes(label = comma(n)), vjust = -1, colour = "black", size = 10)+
            labs(
                title    = paste0("Histogram of ", this_var_name),
                subtitle = paste0(this_cohort, " | ", this_period, 
                                  " | n0 = ", comma(n0), "/", comma(nt), " (", percent(n0/nt), 
                                  ") | nNA = ", comma(nNA), "/", comma(nt), " (", percent(nNA/nt), ")"),
                x        = this_var_name,
                y        = "Count",
            )  +
            ylim(0, y_max) +
            theme_minimal(base_size = 20)

            plot_list[[this_cohort]] <- p
        }
        # paste0("Histograms of ", this_var_name, " | ", this_period, ": Semaglutide vs Other Drugs") %>% cat
        combined <- wrap_plots(plot_list, ncol = 3, byrow = TRUE) +
        plot_annotation(
            title = paste0("Histograms of ", this_var_name, " | ", this_period, ": Semaglutide vs Other Drugs"),
            caption = "Each panel is one comparison drug"
        )

        print(combined)
    }
}

# %%
