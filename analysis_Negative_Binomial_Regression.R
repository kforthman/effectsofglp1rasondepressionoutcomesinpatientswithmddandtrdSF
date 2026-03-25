# -*- coding: utf-8 -*-
# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()
source("helper_functions.R")

# %% [markdown]
# ## Load in cohort definitions

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

# %% [markdown]
# ## Subset dataset for each cohort

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

# dte_cohort_Insulins %>% head

# %% [markdown]
# ## Define periods of interest

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

# %% [markdown]
# ## Read in outcomes

# %%
load("all_outcomes.rds")

# all_outcomes %>% head(20)

# %% [markdown]
# ## Format the outcomes to be merged with the dte dataset

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

# %% [markdown]
# # Drug changes

# %%
#theme_set(theme_sjplot())
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>%
    mutate(treatment_name = factor(treatment_name, levels = c(this_drug, "Semaglutide"))
          )

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

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
    #"\n\nresult:\n" %>% cat
    #summary(m1) %>% print
    
    "\nsummary:\n\n" %>% cat
    interpret_nb(m1, term = "treatment_nameSemaglutide", outcome = "total med changes", comparison = this_drug)
    
    tab <- tab_model(m1)
    display_html(tab$knitr)
    
    p <- plot_model(m1) +
    theme_minimal(base_size = 20) 
    
    #p %>% print()
    
   
    "--------------------------------------------------------------------------------------------" %>% cat
}

# %%
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))
    this_cohort <- paste0("Semaglutide vs ", this_drug)
    this_period <- "15 days-12 months after index"

    my_min <- 1
    my_range <- 1

    n0 <- this_dataset %>% filter(n_med_changes_d == 0) %>% nrow()
    nt <- this_dataset %>% nrow()

    these_levels <- this_dataset %>%
    rowwise %>%
    mutate(value2 = my_cut(n_med_changes_d, my_range, my_min)) %>%
    ungroup() %>%
    arrange(n_med_changes_d) %>% 
    pull(value2) %>% 
    unique

    this_dataset <- this_dataset %>%
    rowwise %>%
    mutate(value2 = my_cut(n_med_changes_d, my_range, my_min)) %>%
    ungroup() %>%
    mutate(value3 = factor(value2, levels = these_levels))

    this_dataset <- this_dataset %>% 
    count(value3, treatment_name) %>% 
    rename(n_med_changes_d = "value3")
    
    this_dataset <- this_dataset %>% 
    mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", this_drug)))
    
    this_dataset_percent <- this_dataset %>% 
    group_by(treatment_name) %>% 
    mutate(my_percent = n/sum(n))
    
    y_max <- max(this_dataset_percent$my_percent, na.rm = T)
    y_max <- y_max*1.1
    
    dodge_width <- 0.9

    p <- ggplot(this_dataset_percent, aes(x = n_med_changes_d, fill = treatment_name, color = treatment_name, y = my_percent)) +
    geom_bar(position = "dodge", stat = "identity")+
    geom_text(aes(label = percent(my_percent)), position = position_dodge(width = dodge_width), vjust = -1, colour = "black", size = 10)+
    labs(
        title    = paste0("Semaglutide vs ", this_drug),
        subtitle = paste0(this_cohort, " | ", this_period, 
                          " | n0 = ", comma(n0), "/", comma(nt), " (", percent(n0/nt), 
                          ")"),
        x        = "n_med_changes_d",
        y        = "Count",
    )  +
    ylim(0, y_max) +
    theme_minimal(base_size = 40)

    print(p)
}

# %% [markdown]
# # Psych visits

# %% [markdown]
# ### Demonstrate the necessity of using negative binomial and run the model
# if overdispersion is over 1, NB is warrented

# %%
#theme_set(theme_sjplot())
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m_pois <- glm(
        n_psych_days_d ~ treatment + race.ethnicity_White + sex_Male + age_at_index_years,
        data = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(n_psych_days_d ~ treatment + race.ethnicity_White + sex_Male + age_at_index_years,
                 data = this_dataset)

    "*Negative Binomial*\n\n" %>% cat
    check_od(m1)
    check_mc(m1)
    # "\n\nresult:\n" %>% cat
    # summary(m1) %>% print
    
    "\nsummary:\n\n" %>% cat
    interpret_nb(m1, term = "treatment", outcome = "total psych days", comparison = this_drug)
    
    tab <- tab_model(m1)
    display_html(tab$knitr)
    
    p <- plot_model(m1) +
    theme_minimal(base_size = 20) 
    
    #p %>% print()
    
   
    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# ### Plot the difference in outcomes between male and female participants

# %%
ggplot(dte_cohort_and_outcomes_Insulins %>% mutate(sex = ifelse(sex_Male == 1, "Male", "Female")), aes(x = n_psych_days_d, fill = sex, color = sex)) +
    geom_histogram() +
    scale_y_log10() +
    theme_minimal(base_size = 20)

# %% [markdown]
# ### Plot the outcome distribution for each comparison.

# %%
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))
    this_cohort <- paste0("Semaglutide vs ", this_drug)
    this_period <- "15 days-12 months after index"

    my_min <- 1
    my_range <- 10

    n0 <- this_dataset %>% filter(n_psych_days_d == 0) %>% nrow()
    nt <- this_dataset %>% nrow()

    these_levels <- this_dataset %>%
    rowwise %>%
    mutate(value2 = my_cut(n_psych_days_d, my_range, my_min)) %>%
    ungroup() %>%
    arrange(n_psych_days_d) %>% 
    pull(value2) %>% 
    unique

    this_dataset <- this_dataset %>%
    rowwise %>%
    mutate(value2 = my_cut(n_psych_days_d, my_range, my_min)) %>%
    ungroup() %>%
    mutate(value3 = factor(value2, levels = these_levels))

    this_dataset <- this_dataset %>% 
    count(value3, treatment_name) %>% 
    rename(n_psych_days_d = "value3")
    
    this_dataset <- this_dataset %>% 
    mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", this_drug)))
    
    y_max <- max(this_dataset$n, na.rm = T)
    y_max <- y_max*1.1
    
    dodge_width <- 0.9

    p <- ggplot(this_dataset, aes(x = n_psych_days_d, fill = treatment_name, color = treatment_name, y = n)) +
    geom_bar(position = "dodge", stat = "identity")+
    geom_text(aes(label = comma(n)), position = position_dodge(width = dodge_width), vjust = -1, colour = "black", size = 10)+
    labs(
        title    = paste0("Semaglutide vs ", this_drug),
        subtitle = paste0(this_cohort, " | ", this_period, 
                          " | n0 = ", comma(n0), "/", comma(nt), " (", percent(n0/nt), 
                          ")"),
        x        = "n_psych_days_d",
        y        = "Count",
    )  +
    ylim(0, y_max) +
    theme_minimal(base_size = 40)
    
    this_dataset_percent <- this_dataset %>% 
    group_by(treatment_name) %>% 
    mutate(my_percent = n/sum(n))
    
    y_max <- max(this_dataset_percent$my_percent, na.rm = T)
    y_max <- y_max*1.1
    
    dodge_width <- 0.9

    p <- ggplot(this_dataset_percent, aes(x = n_psych_days_d, fill = treatment_name, color = treatment_name, y = my_percent)) +
    geom_bar(position = "dodge", stat = "identity")+
    geom_text(aes(label = percent(my_percent)), position = position_dodge(width = dodge_width), vjust = -1, colour = "black", size = 10)+
    labs(
        title    = paste0("Semaglutide vs ", this_drug),
        subtitle = paste0(this_cohort, " | ", this_period, 
                          " | n0 = ", comma(n0), "/", comma(nt), " (", percent(n0/nt), 
                          ")"),
        x        = "n_psych_days_d",
        y        = "Count",
    )  +
    ylim(0, y_max) +
    theme_minimal(base_size = 40)


    print(p)
}

# %%
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug))
    this_cohort <- paste0("Semaglutide vs ", this_drug)
    this_period <- "15 days-12 months after index"
    
    this_dataset <- this_dataset %>%
    mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", this_drug)))

    n0 <- this_dataset %>% filter(n_psych_days_d == 0) %>% nrow()
    nt <- this_dataset %>% nrow()

    p <- ggplot(this_dataset, aes(x = n_psych_days_d, fill = treatment_name, color = treatment_name)) +
    geom_density(alpha = 0.5)+
    labs(
        title    = paste0("Semaglutide vs ", this_drug),
        subtitle = paste0(this_cohort, " | ", this_period, 
                          " | n0 = ", comma(n0), "/", comma(nt), " (", percent(n0/nt), 
                          ")"),
        x        = "n_psych_days_d",
        y        = "Count",
    )  +
    theme_minimal(base_size = 40)

    print(p)
}

# %% [markdown]
# ## Sensitivity analysis

# %% [markdown]
# ### Run the NB for females only

# %%
# Females
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>% filter(sex_Male == 0)

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m_pois <- glm(
        n_psych_days_d ~ treatment + race.ethnicity_White + age_at_index_years,
        data = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(n_psych_days_d ~ treatment + race.ethnicity_White + age_at_index_years,
                 data = this_dataset)

    "*Negative Binomial*\n\n" %>% cat
    check_od(m1)
    check_mc(m1)
    # "\n\nresult:\n" %>% cat
    # summary(m1) %>% print
    
    "\nsummary:\n\n" %>% cat
    interpret_nb(m1, term = "treatment", outcome = "total psych days", comparison = this_drug)
    
    tab <- tab_model(m1)
    display_html(tab$knitr)
    
    p <- plot_model(m1) +
    theme_minimal(base_size = 20) 
    
    #p %>% print()
    
   
    "--------------------------------------------------------------------------------------------" %>% cat
}

# %% [markdown]
# ### Run the NB for males only

# %%
# Males
options(repr.plot.width=37, repr.plot.height=12)
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    this_dataset <- get(paste0("dte_cohort_and_outcomes_", this_drug)) %>% filter(sex_Male == 1)

    paste0("\n\n**Semaglutide vs ", this_drug, "**\n\n") %>% cat

    m_pois <- glm(
        n_psych_days_d ~ treatment + race.ethnicity_White + age_at_index_years,
        data = this_dataset,
        family = poisson()
    )

    check_poisson(m_pois)

    m1 <- glm.nb(n_psych_days_d ~ treatment + race.ethnicity_White + age_at_index_years,
                 data = this_dataset)

    "*Negative Binomial*\n\n" %>% cat
    check_od(m1)
    check_mc(m1)
    # "\n\nresult:\n" %>% cat
    # summary(m1) %>% print
    
    "\nsummary:\n\n" %>% cat
    interpret_nb(m1, term = "treatment", outcome = "total psych days", comparison = this_drug)
    
    tab <- tab_model(m1)
    display_html(tab$knitr)
    
    p <- plot_model(m1) +
    theme_minimal(base_size = 20) 
    
    #p %>% print()
    
   
    "--------------------------------------------------------------------------------------------" %>% cat
}

# %%
