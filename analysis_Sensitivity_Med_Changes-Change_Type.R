# -*- coding: utf-8 -*-
# %% [markdown]
# # Setup

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()
source("helper_functions.R")

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
# # Analysis

# %% [markdown]
# ## Decern what type of medication changes are occurring
#
# Goal: Characterize what types of medication changes are occurring in each treatment group
# during the 15 days–12 months post-index follow-up window.
#
# Method: Classify each medication event in the consecutive treatment period table as one of:
#
# - New regimen: a new treatment period with no prior event as reference
# - Switch: replacement of one medication with another
# - Addition: adding a medication to an existing regimen
# - Antipsychotic augmentation: addition of an antipsychotic to an existing antidepressant regimen
#
# Among addition events, examine whether the added drug is within the same chemical class or
# a different chemical class than the reference medication.
#
# Among switch events, examine whether the switch is within the same chemical subgroup
# (e.g., SSRI → SSRI) or across subgroups (e.g., SSRI → SNRI).
#
# At the person level, classify each participant's overall pattern as: no events, new regimen
# only, changes to an existing regimen (additions/switches), or both.
#
# Interpretation: Differences in the distribution of event types between Semaglutide and
# comparator groups may reflect differences in treatment trajectory — e.g., more
# antipsychotic augmentations in one group would suggest greater treatment complexity.

# %% [markdown]
# ### Plot 1: Treatment-type event distribution 
# Stacked proportional bar: what share of medication events in the follow-up window
# are new_regimen / switch / antidepressant addition / antipsychotic augmentation
# for Semaglutide vs. the comparator?

# %%
options(repr.plot.width = 37, repr.plot.height = 12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_data <- get(paste0("consecutive_period_", this_drug)) %>%
        filter(!is.na(treatment_type)) %>%
        mutate(event_type = case_when(
            treatment_type == "addition" & concept_category_level_3 == "ANTIPSYCHOTICS" ~ "Antipsychotic augmenter",
            treatment_type == "addition"                                                ~ "Addition",
            treatment_type == "switch"                                                  ~ "Switch",
            treatment_type == "new_regimen"                                             ~ "New regimen",
            treatment_type == "removal"                                                 ~ "Removal"
        ))

    type_summary <- this_data %>%
        count(treatment_name, event_type) %>%
        group_by(treatment_name) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup() %>%
        mutate(event_type = factor(event_type,
                                   levels = c("New regimen", "Switch", "Addition", "Antipsychotic augmenter", "Removal")))

    bar_totals <- type_summary %>%
        group_by(treatment_name) %>%
        summarise(total_n = sum(n), .groups = "drop")

    participant_totals <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
        count(treatment_name, name = "total_n")

    p <- ggplot(type_summary, aes(x = treatment_name, y = prop, fill = event_type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 8, color = "white", fontface = "bold") +
        geom_text(data = bar_totals, aes(x = treatment_name, y = 1, label = paste0("N = ", scales::comma(total_n))),
                  inherit.aes = FALSE, vjust = -0.5, size = 7, fontface = "bold") +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.08))) +
        scale_x_discrete(labels = setNames(paste0(participant_totals$treatment_name, "\n(n = ", scales::comma(participant_totals$total_n), ")"),
                                           participant_totals$treatment_name)) +
        scale_fill_brewer(palette = "Set2") +
        labs(
            title    = paste0("Medication event types: Semaglutide vs ", this_drug),
            subtitle = "15 days–12 months after index | all medication events",
            x = NULL, y = "Proportion of events", fill = "Event type"
        ) +
        theme_minimal(base_size = 30)

    print(p)
}

# %% [markdown]
# ### Plot 2: Chemical class rate among additions
# Top panel: proportion of additions that cross chemical class (e.g. adding an
#             SNRI to an SSRI regimen) — Semaglutide vs. comparator.
# Right panel: breakdown of which chemical class is being added.

# %%
options(repr.plot.width = 37, repr.plot.height = 12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_data <- get(paste0("consecutive_period_", this_drug)) %>%
        filter(treatment_type == "addition", !is.na(different_pharmacological_subgroup), concept_category_level_3 == "ANTIDEPRESSANTS")

    if(nrow(this_data) == 0){
        paste0("No addition events found for Semaglutide vs ", this_drug, "\n") %>% cat
        next
    }

    # Cross-class augmentation rate
    aug_rate <- this_data %>%
        count(treatment_name, different_chemical_subgroup) %>%
        group_by(treatment_name) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup() %>%
        mutate(label = ifelse(different_chemical_subgroup,
                              "Different chem. class", "Same chem. class"))

    bar_totals <- this_data %>% count(treatment_name, name = "total_n")

    participant_totals <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
        count(treatment_name, name = "total_n")

    p1 <- ggplot(aug_rate, aes(x = treatment_name, y = prop, fill = label)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 8, color = "white", fontface = "bold") +
        geom_text(data = bar_totals, aes(x = treatment_name, y = 1, label = paste0("N = ", scales::comma(total_n))),
                  inherit.aes = FALSE, vjust = -0.5, size = 7, fontface = "bold") +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.08))) +
        scale_x_discrete(labels = setNames(paste0(participant_totals$treatment_name, "\n(n = ", scales::comma(participant_totals$total_n), ")"),
                                           participant_totals$treatment_name)) +
        scale_fill_manual(values = c("Different chem. class" = "#E41A1C", "Same chem. class" = "#377EB8")) +
        labs(
            title    = paste0("Cross-class augmentation: Semaglutide vs ", this_drug),
            subtitle = "Among addition events — is the added drug a different chemical class?",
            x = NULL, y = "Proportion of additions", fill = NULL
        ) +
        theme_minimal(base_size = 30)

    # Breakdown by class being added
    class_added <- this_data %>%
        count(treatment_name, concept_category_level_5) %>%
        group_by(treatment_name) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup()

    p2 <- ggplot(class_added, aes(x = treatment_name, y = prop, fill = concept_category_level_5)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 7, color = "white", fontface = "bold") +
        geom_text(data = bar_totals, aes(x = treatment_name, y = 1, label = paste0("N = ", scales::comma(total_n))),
                  inherit.aes = FALSE, vjust = -0.5, size = 7, fontface = "bold") +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.08))) +
        scale_x_discrete(labels = setNames(paste0(participant_totals$treatment_name, "\n(n = ", scales::comma(participant_totals$total_n), ")"),
                                           participant_totals$treatment_name)) +
        scale_fill_brewer(palette = "Set2") +
        labs(
            title    = paste0("Drug class added: Semaglutide vs ", this_drug),
            subtitle = "Among addition events",
            x = NULL, y = "Proportion of additions", fill = "Drug class added"
        ) +
        theme_minimal(base_size = 30)

    print(p1)
    print(p2)
}

# %% [markdown]
# ### Plot 3: Within vs. across chemical subgroup switches
# Among switch events only: proportion that switch to a different chemical subgroup
# (e.g. SSRI → SNRI) vs. staying within the same subgroup (e.g. SSRI → SSRI).
# Also shows a breakdown of which chemical class is being switched to.

# %%
options(repr.plot.width = 37, repr.plot.height = 12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_data <- get(paste0("consecutive_period_", this_drug)) %>%
        filter(treatment_type == "switch", !is.na(different_chemical_subgroup))

    if(nrow(this_data) == 0){
        paste0("No switch events found for Semaglutide vs ", this_drug, "\n") %>% cat
        next
    }

    switch_summary <- this_data %>%
        count(treatment_name, different_chemical_subgroup) %>%
        group_by(treatment_name) %>%
        mutate(prop  = n / sum(n),
               label = ifelse(different_chemical_subgroup,
                              "Different chemical subgroup", "Same chemical subgroup")) %>%
        ungroup()

    bar_totals <- this_data %>% count(treatment_name, name = "total_n")

    participant_totals <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
        count(treatment_name, name = "total_n")

    p1 <- ggplot(switch_summary, aes(x = treatment_name, y = prop, fill = label)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 8, color = "white", fontface = "bold") +
        geom_text(data = bar_totals, aes(x = treatment_name, y = 1, label = paste0("N = ", scales::comma(total_n))),
                  inherit.aes = FALSE, vjust = -0.5, size = 7, fontface = "bold") +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.08))) +
        scale_x_discrete(labels = setNames(paste0(participant_totals$treatment_name, "\n(n = ", scales::comma(participant_totals$total_n), ")"),
                                           participant_totals$treatment_name)) +
        scale_fill_manual(values = c("Different chemical subgroup" = "#E41A1C",
                                     "Same chemical subgroup"      = "#4DAF4A")) +
        labs(
            title    = paste0("Switch type by chemical subgroup: Semaglutide vs ", this_drug),
            subtitle = "Among switch events — within-subgroup (e.g. SSRI→SSRI) vs. across-subgroup (e.g. SSRI→SNRI)",
            x = NULL, y = "Proportion of switches", fill = NULL
        ) +
        theme_minimal(base_size = 30)

    # Breakdown by chemical class being switched to
    class_switched <- this_data %>%
        count(treatment_name, concept_category_level_5) %>%
        group_by(treatment_name) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup()

    p2 <- ggplot(class_switched, aes(x = treatment_name, y = prop, fill = concept_category_level_5)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 7, color = "white", fontface = "bold") +
        geom_text(data = bar_totals, aes(x = treatment_name, y = 1, label = paste0("N = ", scales::comma(total_n))),
                  inherit.aes = FALSE, vjust = -0.5, size = 7, fontface = "bold") +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.08))) +
        scale_x_discrete(labels = setNames(paste0(participant_totals$treatment_name, "\n(n = ", scales::comma(participant_totals$total_n), ")"),
                                           participant_totals$treatment_name)) +
        scale_fill_brewer(palette = "Set2") +
        labs(
            title    = paste0("Drug class switched to: Semaglutide vs ", this_drug),
            subtitle = "Among switch events",
            x = NULL, y = "Proportion of switches", fill = "Drug class switched to"
        ) +
        theme_minimal(base_size = 30)

    print(p1)
    print(p2)
}

# %% [markdown]
# ### Plot 4: Person-level medication pattern during follow-up
# For each participant, characterise their pattern:
#   "No events"            — no medication events in the follow-up window
#   "New regimen only"     — all events are new_regimen
#   "Changes to existing"  — all events are addition, switch, or removal
#   "Both"                 — has at least one new_regimen AND at least one addition/switch/removal

# %%
options(repr.plot.width = 37, repr.plot.height = 12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug   <- drug_matchedDS_info$drug[i]
    this_cohort <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
        dplyr::select(person_id, treatment_name) %>%
        mutate(treatment_name = factor(treatment_name, levels = c(this_drug, "Semaglutide")))
    this_data   <- get(paste0("consecutive_period_", this_drug))

    person_pattern <- this_data %>%
        group_by(person_id, treatment_name) %>%
        summarise(
            has_new_regimen = any(treatment_type == "new_regimen", na.rm = TRUE),
            has_change      = any(treatment_type %in% c("addition", "switch", "removal"), na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(pattern = case_when(
            has_new_regimen &  has_change ~ "Both",
            has_new_regimen & !has_change ~ "New regimen only",
           !has_new_regimen &  has_change ~ "Changes to existing",
            TRUE                          ~ "No events"
        ))

    # Participants with no events in the window
    no_events <- this_cohort %>%
        anti_join(person_pattern, by = "person_id") %>%
        mutate(pattern = "No events")

    person_pattern <- bind_rows(
        person_pattern %>% dplyr::select(person_id, treatment_name, pattern),
        no_events
    )

    pattern_lvls <- c("No events", "New regimen only", "Changes to existing", "Both")

    pattern_summary <- person_pattern %>%
        count(treatment_name, pattern) %>%
        group_by(treatment_name) %>%
        mutate(prop    = n / sum(n),
               pattern = factor(pattern, levels = pattern_lvls)) %>%
        ungroup()

    bar_totals <- pattern_summary %>%
        group_by(treatment_name) %>%
        summarise(total_n = sum(n), .groups = "drop")

    p <- ggplot(pattern_summary, aes(x = treatment_name, y = prop, fill = pattern)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 7, color = "white", fontface = "bold") +
        geom_text(data = bar_totals, aes(x = treatment_name, y = 1, label = paste0("N = ", scales::comma(total_n))),
                  inherit.aes = FALSE, vjust = -0.5, size = 7, fontface = "bold") +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.08))) +
        scale_x_discrete(labels = setNames(paste0(bar_totals$treatment_name, "\n(n = ", scales::comma(bar_totals$total_n), ")"),
                                           bar_totals$treatment_name)) +
        scale_fill_brewer(palette = "Set2") +
        labs(
            title    = paste0("Person-level medication pattern: Semaglutide vs ", this_drug),
            subtitle = "15 days–12 months after index",
            x = NULL, y = "Proportion of participants", fill = "Pattern"
        ) +
        theme_minimal(base_size = 30)

    print(p)
}
