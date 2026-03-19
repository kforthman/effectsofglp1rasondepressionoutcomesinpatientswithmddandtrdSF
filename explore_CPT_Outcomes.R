# -*- coding: utf-8 -*-
# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

dte_cohort_data %>% head(10)

# %%
load("dte_cohort_psych_visits.rds")

dte_cohort_psych_visits <- psych_visit_data

dte_cohort_psych_visits %>% head(10)

# %%
drug_matchedDS_info <- c("Insulins", "PS_Matched_Dataset-Insulins.rds",
                        "Metformin", "PS_Matched_Dataset-Metformin.rds",
                        "DPP4i", "PS_Matched_Dataset-DPP4i.rds",
                        "SGLT2i", "PS_Matched_Dataset-SGLT2i.rds",
                        "SU", "PS_Matched_Dataset-SU.rds",
                        "TZD", "PS_Matched_Dataset-TZD.rds",
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
    dplyr::select(person_id) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))


    var_name <- paste0("dte_cohort_ids_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, matched.data)
}

# %%
dte_cohort_ids_allSets <- rbind(
    dte_cohort_ids_Insulins,
    dte_cohort_ids_Metformin,
    dte_cohort_ids_DPP4i,
    dte_cohort_ids_SGLT2i,
    dte_cohort_ids_SU,
    dte_cohort_ids_TZD,
    dte_cohort_ids_GLP1RA,
    dte_cohort_ids_Nontreatment
)

dte_cohort_ids_allSets <- dte_cohort_ids_allSets %>% mutate(study_cohort = as.factor(study_cohort))

dte_cohort_ids_allSets %>% head(10)

dte_cohort_ids_allSets %>% dim

dte_cohort_ids_allSets$study_cohort %>% table()

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i])

    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")
    
    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext))))

    dte_cohort_psych_visits_this_drug <- dte_cohort_psych_visits %>% 
    filter(person_id %in% matched.data$person_id) %>%
    left_join(matched.data %>% dplyr::select(person_id, treatment, treatment_name, first_drug_record), by = join_by(person_id == person_id)) %>%
    mutate(time_to_visit = interval(first_drug_record, start_datetime) / days(1)) %>%
    filter(time_to_visit>=0 & time_to_visit <=365) %>%
    mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", drug_matchedDS_info$drug[i]))) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))

    var_name <- paste0("dte_cohort_psych_visits_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, dte_cohort_psych_visits_this_drug)
}

# %%
dte_cohort_psych_visits_allSets <- rbind(
    dte_cohort_psych_visits_Insulins,
    dte_cohort_psych_visits_Metformin,
    dte_cohort_psych_visits_DPP4i,
    dte_cohort_psych_visits_SGLT2i,
    dte_cohort_psych_visits_SU,
    dte_cohort_psych_visits_TZD,
    dte_cohort_psych_visits_GLP1RA,
    dte_cohort_psych_visits_Nontreatment
)

dte_cohort_psych_visits_allSets <- dte_cohort_psych_visits_allSets %>% mutate(study_cohort = as.factor(study_cohort))

dte_cohort_psych_visits_allSets %>% head(10)

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
dte_cohort_psych_visits_allSets %>% head

# %%
options(repr.plot.width=20, repr.plot.height=12)

time_to_first_crisis_psych <- dte_cohort_psych_visits_allSets %>% 
filter(source_concept_code %in% c("90839", "90840")) %>%
group_by(person_id, study_cohort) %>%
summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
ungroup() %>%
arrange(-time_to_first_visit)

time_to_first_crisis_psych %>% head
time_to_first_crisis_psych$time_to_first_visit %>% max

my_histogram(time_to_first_crisis_psych, "time_to_first_visit", 
             title = "Histogram of Time to First Crisis Psychiatric or Psychological Visit", 
             xlab = "Total Days to First Psychiatric or Psychological Visit", 
             ylab = "Number of Patients",
             xscalefactor = 2)

# %%
options(repr.plot.width=35, repr.plot.height=12)
plot_list <- list()

for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    if(this_drug == "Semaglutide"){next}

    these_visits <- get(paste0("dte_cohort_psych_visits_", this_drug))
    these_ids <- get(paste0("dte_cohort_ids_", this_drug))
    
    these_visits <- these_visits %>% 
    filter(source_concept_code %in% c("90839", "90840"))

    if(nrow(these_visits) > 0){
        time_to_first_crisis_psych <- these_visits %>% 
        group_by(person_id) %>%
        summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
        ungroup() %>%
        arrange(-time_to_first_visit)

        n0 <- sum(these_ids$person_id %in% time_to_first_crisis_psych$person_id)
    }else{n0 <- 0}
    
    nt <- nrow(these_ids)

    cat(paste0("\n\nSemaglutide vs ", this_drug, " cohort"))
    cat(paste0("\nTotal individuals with a crisis psychotherapy event (CPT 90839/90840)"))
    cat(paste0("\n", n0, " / ", nt))

}


# %%
options(repr.plot.width=20, repr.plot.height=12)

time_to_first_somatic_psych <- dte_cohort_psych_visits_allSets %>% 
filter(source_concept_code %in% c("90867", "90868", "90869", "90870")) %>%
group_by(person_id, study_cohort) %>%
summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
ungroup() %>%
arrange(-time_to_first_visit)

time_to_first_somatic_psych %>% head
time_to_first_somatic_psych$time_to_first_visit %>% max

my_histogram(time_to_first_somatic_psych, "time_to_first_visit", 
             title = "Histogram of Time to First ECT/TMS Visit", 
             xlab = "Total Days to First Psychiatric or Psychological Visit", 
             ylab = "Number of Patients",
             xscalefactor = 2)

# %%
options(repr.plot.width=35, repr.plot.height=12)
plot_list <- list()

for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    if(this_drug == "Semaglutide"){next}

    these_visits <- get(paste0("dte_cohort_psych_visits_", this_drug))
    these_ids <- get(paste0("dte_cohort_ids_", this_drug))

    time_to_first_somatic_psych <- these_visits %>% 
    filter(source_concept_code %in% c("90867", "90868", "90869", "90870")) %>% 
    group_by(person_id) %>%
    summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
    ungroup() %>%
    arrange(-time_to_first_visit)
    
    n0 <- sum(these_ids$person_id %in% time_to_first_somatic_psych$person_id)
    nt <- nrow(these_ids)
    
    cat(paste0("\n\nSemaglutide vs ", this_drug, " cohort"))
    cat(paste0("\nTotal individuals with a somatic psychotherapy event, i.e. ECT/TMS (CPT 90867/90868/90869/90870)"))
    cat(paste0("\n", n0, " / ", nt))

}

# %%
options(repr.plot.width=20, repr.plot.height=12)

time_to_first_level5_psych <- dte_cohort_psych_visits_allSets %>% 
filter(level == 5) %>%
group_by(person_id, study_cohort) %>%
summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
ungroup() %>%
arrange(-time_to_first_visit)

time_to_first_level5_psych %>% head
time_to_first_level5_psych$time_to_first_visit %>% max

my_histogram(time_to_first_level5_psych, "time_to_first_visit", 
             title = "Histogram of Time to First Level 5 Psychotherapy Event", 
             xlab = "Total Days to First Psychiatric or Psychological Visit", 
             ylab = "Number of Patients",
             xscalefactor = 2)

# %%
options(repr.plot.width=35, repr.plot.height=12)
plot_list <- list()

for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    if(this_drug == "Semaglutide"){next}

    these_visits <- get(paste0("dte_cohort_psych_visits_", this_drug))
    these_ids <- get(paste0("dte_cohort_ids_", this_drug))

    these_visits <- these_visits %>% 
    filter(level == 5)

    if(nrow(these_visits) > 0){
        time_to_first_level5_psych <- these_visits %>%
        group_by(person_id) %>%
        summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
        ungroup() %>%
        arrange(-time_to_first_visit)

        n0 <- sum(these_ids$person_id %in% time_to_first_level5_psych$person_id)
    }else{n0 <- 0}

    nt <- nrow(these_ids)

    cat(paste0("\n\nSemaglutide vs ", this_drug, " cohort"))
    cat(paste0("\nTotal individuals with a level 5 psychotherapy event"))
    cat(paste0("\n", n0, " / ", nt))

}

# %%
options(repr.plot.width=20, repr.plot.height=12)

time_to_first_acute_psych <- dte_cohort_psych_visits_allSets %>% 
filter(level >= 4) %>%
group_by(person_id, study_cohort) %>%
summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
ungroup() %>%
arrange(-time_to_first_visit)

time_to_first_acute_psych %>% head
time_to_first_acute_psych$time_to_first_visit %>% max

my_histogram(time_to_first_acute_psych, "time_to_first_visit", 
             title = "Histogram of Time to First Acute Psychotherapy Event (Level > 3)", 
             xlab = "Total Days to First Psychiatric or Psychological Visit", 
             ylab = "Number of Patients",
             xscalefactor = 2)

# %%
options(repr.plot.width=35, repr.plot.height=12)
plot_list <- list()

for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    if(this_drug == "Semaglutide"){next}

    these_visits <- get(paste0("dte_cohort_psych_visits_", this_drug))
    these_ids <- get(paste0("dte_cohort_ids_", this_drug))

    these_visits <- these_visits %>% 
    filter(level >= 4)

    if(nrow(these_visits) > 0){
        time_to_first_acute_psych <- these_visits %>%
        group_by(person_id) %>%
        summarize(time_to_first_visit = min(time_to_visit), .groups = "keep") %>%
        ungroup() %>%
        arrange(-time_to_first_visit)

        n0 <- sum(these_ids$person_id %in% time_to_first_acute_psych$person_id)
    }else{n0 <- 0}

    nt <- nrow(these_ids)

    cat(paste0("\n\nSemaglutide vs ", this_drug, " cohort"))
    cat(paste0("\nTotal individuals with an acute psychotherapy event (Level > 3)"))
    cat(paste0("\n", n0, " / ", nt))

}

# %%
