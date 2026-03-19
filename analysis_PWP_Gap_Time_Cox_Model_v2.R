# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

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

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i])
    
    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")

    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext)))) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))


    var_name <- paste0("dte_cohort_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, matched.data)
}

dte_cohort_Insulins %>% head

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
load("dte_cohort_psych_visits.rds")

dte_cohort_psych_visits <- psych_visit_data

dte_cohort_psych_visits %>% head(10)

# %%
load("antidepressant_antipsychotic_consecutive_period.rds", verbose = TRUE)

dte_cohort_drug_use <- consecutive_period_tab

dte_cohort_drug_use %>% head

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i]) # matched.data

    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")
    
    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext))))

    dte_cohort_psych_visits_this_drug <- dte_cohort_psych_visits %>% 
    filter(person_id %in% matched.data$person_id) %>%
    left_join(matched.data %>% dplyr::select(person_id, treatment, treatment_name, first_drug_record), by = join_by(person_id == person_id)) %>%
    mutate(time_from_index_days = time_length(interval(first_drug_record, start_datetime), "days")) %>%
    mutate(time_from_index_months = time_length(interval(first_drug_record, start_datetime), "months")) %>%
    mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", drug_matchedDS_info$drug[i]))) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))

    var_name <- paste0("dte_cohort_psych_visits_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, dte_cohort_psych_visits_this_drug)
    
    "\n" %>% cat
}

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i]) # matched.data

    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")
    
    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext))))

    dte_cohort_drug_use_this_drug <- dte_cohort_drug_use %>% 
    filter(person_id %in% matched.data$person_id) %>%
    left_join(matched.data %>% dplyr::select(person_id, treatment, treatment_name, first_drug_record), by = join_by(person_id == person_id)) %>%
    mutate(time_from_index_days = time_length(interval(first_drug_record, first_record), "days")) %>%
    mutate(time_from_index_months = time_length(interval(first_drug_record, first_record), "months")) %>%
    mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", drug_matchedDS_info$drug[i]))) %>%
    mutate(n_days =  time_length(interval(first_record, last_record), "days") + 1) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))

    var_name <- paste0("dte_cohort_drug_use_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, dte_cohort_drug_use_this_drug)
    
    "\n" %>% cat
}

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_", this_drug))
    this_psych_data <- get(paste0("dte_cohort_psych_visits_", this_drug))
    these_ids <- this_dataset$person_id

    bgn_win <- 15
    end_win <- 365
    
    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    out1 <- this_psych_data %>% 
    filter(person_id %in% these_ids) %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    arrange(person_id, time_from_index_days)%>%
    group_by(person_id, time_from_index_days) %>%
    slice_head(n = 1) %>%
    ungroup %>%
    dplyr::select(person_id, time_from_index_days) %>%
    mutate(event = "psych_visit") %>%
    rename(event_time = "time_from_index_days")

    out2 <- data.frame(person_id = these_ids, event = "end_followup", event_time = end_win)

    out <- rbind(out1, out2) %>%
    arrange(person_id, event_time) %>%
    mutate(status = ifelse(event == "end_followup", 0,1)) %>%
    group_by(person_id) %>%
    mutate(
        start = lag(event_time, default = 0),
        stop  = event_time,
        event_num = row_number(),
        gap_start = 0,
        gap = stop - start
    ) %>%
    ungroup() %>% 
    left_join(this_dataset %>% dplyr::select(person_id, treatment, race.ethnicity_White, sex_Male, age_at_index_years),
              by = "person_id")

    fit <- coxph(
        Surv(gap_start, gap, status) ~ treatment + race.ethnicity_White + sex_Male + age_at_index_years + strata(event_num) + cluster(person_id),
        data = out
    )

    # print(fit)
    summary(fit) %>% print()
    
    "--------------------------------------------------------------------------------------------" %>% cat
}

# %%
cov_ref <- out %>% 
  summarize(
    race.ethnicity_White = as.integer(round(mean(race.ethnicity_White, na.rm=TRUE))),
    sex_Male             = as.integer(round(mean(sex_Male, na.rm=TRUE))),
    age_at_index_years   = mean(age_at_index_years, na.rm=TRUE)
  )

dummy <- expand.grid(race.ethnicity_White=c(0,1), 
                     sex_Male=c(0,1),
                     treatment=c(0,1),
                     event_num = c(1,2,3)
                    )

newdat <- bind_rows(
  tibble(treatment = 0, event_num = 1),
  tibble(treatment = 1, event_num = 1),
  tibble(treatment = 0, event_num = 2),
  tibble(treatment = 1, event_num = 2),
  tibble(treatment = 0, event_num = 3),
  tibble(treatment = 1, event_num = 3)
) %>%
  crossing(cov_ref)

sf <- survfit(fit, newdata = newdat)

# %%
out %>% head(30)

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_", this_drug))
    this_drug_data <- get(paste0("dte_cohort_drug_use_", this_drug))
    these_ids <- this_dataset$person_id

    bgn_win <- 15
    end_win <- 365
    
    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    out1 <- this_drug_data %>% 
    filter(person_id %in% these_ids) %>%
    filter(time_from_index_days >= bgn_win & time_from_index_days < end_win) %>% 
    arrange(person_id, time_from_index_days)%>%
    # group_by(person_id, time_from_index_days) %>%
    # slice_head(n = 1) %>%
    # ungroup %>%
    dplyr::select(person_id, time_from_index_days) %>%
    mutate(event = "drug_change") %>%
    rename(event_time = "time_from_index_days")

    out2 <- data.frame(person_id = these_ids, event = "end_followup", event_time = end_win)

    out <- rbind(out1, out2) %>%
    arrange(person_id, event_time) %>%
    mutate(status = ifelse(event == "end_followup", 0,1)) %>%
    group_by(person_id) %>%
    mutate(
        start = lag(event_time, default = 0),
        stop  = event_time,
        event_num = row_number(),
        gap_start = 0,
        gap = stop - start
    ) %>%
    ungroup() %>% 
    left_join(this_dataset %>% dplyr::select(person_id, treatment, race.ethnicity_White, sex_Male, age_at_index_years),
              by = "person_id")

    fit <- coxph(
        Surv(gap_start, gap, status) ~ treatment + race.ethnicity_White + sex_Male + age_at_index_years + strata(event_num) + cluster(person_id),
        data = out
    )

    # print(fit)
    summary(fit) %>% print()
    
    "--------------------------------------------------------------------------------------------" %>% cat
}

# %%
out %>% head(30)

# %%
