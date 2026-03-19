# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("Data_Prepped/Prepped_Data-All_Participants-All_Visits-Visit.rds")
visit_record <- data_prep

visit_record %>% head

# %%
visit_record %>% head

# %%
all_cohort_ids <- visit_record %>% dplyr::select(person_id) %>% group_by(person_id) %>% slice(1) %>% ungroup

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
    
    var_name <- paste0(drug_timeline_info$drug[i], "_data")
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, data_prep)
}

# %%
all_drug_ids <- data.frame(person_id = c(
    Semaglutide_data$person_id,
    Insulins_data$person_id,
    Metformin_data$person_id,
    DPP4i_data$person_id,
    SGLT2i_data$person_id,
    SU_data$person_id,
    TZD_data$person_id,
    GLP1RA_data$person_id
    )
) %>% 
group_by(person_id) %>% 
slice(1) %>% 
ungroup

all_drug_ids %>% head

# %%
visit_record_drug_subset <- visit_record %>%
filter(person_id %in% all_drug_ids$person_id)

# %%
overwrite <- FALSE
# Loop over each file path
for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]
    
    var_name <- paste0(this_drug, "_data")
    this_data <- get(var_name)
    these_drug_ids <- this_data %>% dplyr::select(person_id) %>% group_by(person_id) %>% slice(1) %>% ungroup
    
    flush.console()
    cat(paste0("\n\n", this_drug, " (n = ", comma(nrow(these_drug_ids)), ")\n\n"))
        
    if(file.exists(paste0(this_drug, "_index_info.rds")) & !overwrite){
        cat(paste0("Skipping file creation, file already exists. Loading existing file."))
        load(paste0(this_drug, "_index_info.rds"))
        var_name <- paste0(this_drug, "_index_info")
        paste0("\nCreating variable ", var_name, "\n") %>% cat
        assign(var_name, this_index_info)
        next
            }
    
    this_visit_record_drug_subset <- visit_record_drug_subset %>%
    filter(person_id %in% these_drug_ids$person_id)

    this_index_info <- this_data %>% 
    dplyr::select(person_id, start_datetime) %>%
    mutate(has_visit_data = person_id %in% all_cohort_ids$person_id) %>%
    rename(drug_start_datetime = start_datetime) %>%
    group_by(person_id) %>% 
    filter(drug_start_datetime == min(drug_start_datetime)) %>% 
    rename(drug_index_datetime = drug_start_datetime) %>%
    slice(1) %>%
    ungroup()

    this_index_info$visit_in_3_to_0_m_prior <- NA
    this_index_info$visit_in_6_to_3_m_prior <- NA
    
    tot <- nrow(this_index_info)
    new_progress <- -1
    for(j in 1:tot){
        flush.console()
        current_progress <- new_progress
        new_progress <- round(j / tot, 2)*100
        if(new_progress > current_progress){cat(paste0(new_progress, "%\t"))}
        
        if(!this_index_info$has_visit_data[j]){next}
        this_person <- this_index_info$person_id[j]
        this_index <- this_index_info$drug_index_datetime[j]
        this_visit_record <- this_visit_record_drug_subset %>% 
        filter(person_id == this_person) %>%
        mutate(drug_index_datetime = this_index,
               time_from_index = time_length(interval(drug_index_datetime, start_datetime), "days")
              ) 

        visit_in_3_to_0_m_prior <- this_visit_record %>%
        filter(time_from_index >= -90 & time_from_index <= -1) %>%
        nrow

        visit_in_6_to_3_m_prior <- this_visit_record %>%
        filter(time_from_index >= -180 & time_from_index <= -91) %>%
        nrow

        this_index_info$visit_in_3_to_0_m_prior[j] <- visit_in_3_to_0_m_prior
        this_index_info$visit_in_6_to_3_m_prior[j] <- visit_in_6_to_3_m_prior

    }
    
    var_name <- paste0(this_drug, "_index_info")
    paste0("\nCreating variable ", var_name, "\n") %>% cat
    assign(var_name, this_index_info)
    save(this_index_info, file = paste0(var_name, ".rds"))
}

# %%
for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]
    var_name <- paste0(this_drug, "_index_info")
    this_data <- get(var_name)

    this_data <- this_data %>%
    replace_na(list(visit_in_3_to_0_m_prior = 0, 
                    visit_in_6_to_3_m_prior = 0)) %>%
    mutate(!!sym(paste0(this_drug, "_meets_visit_criteria")) := visit_in_3_to_0_m_prior > 0 & visit_in_6_to_3_m_prior > 0) %>%
    dplyr::select(-drug_index_datetime, -has_visit_data, -visit_in_3_to_0_m_prior, -visit_in_6_to_3_m_prior)
    
    if(i == 1){
        all.drug.data <- this_data
    }else{
        all.drug.data <- all.drug.data %>%
        full_join(this_data, by = join_by(person_id == person_id))
    }
}

# %%
all.drug.data <- all.drug.data %>%
mutate(across(ends_with("_meets_visit_criteria"), ~replace_na(., FALSE))) %>%
arrange(person_id)

all.drug.data %>% head

# %%
save(all.drug.data, file = "data_DTE_VisitTimelineVars.rds")

# %%
load("dte_cohort_data.rds")
dte_cohort_data <- this.data

# %%
dte_cohort_data.2 <- dte_cohort_data %>%
left_join(all.drug.data, by = "person_id")

# %%
dte_cohort_data.3 <- dte_cohort_data.2

for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]

    if(this_drug == "Semaglutide"){next}

    var_name_meets_visit_criteria <- paste0(this_drug, "_meets_visit_criteria")
    var_name_semaglutide_pop <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    var_name_other_drug_pop <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    dte_cohort_data.3 <- dte_cohort_data.3 %>%
    mutate(!!sym(var_name_semaglutide_pop) := !!sym(var_name_semaglutide_pop) & Semaglutide_meets_visit_criteria,
           !!sym(var_name_other_drug_pop) := !!sym(var_name_other_drug_pop) & !!sym(var_name_meets_visit_criteria)
          )

}

# %%
cohort_names <- do.call(c, lapply(
    drug_timeline_info %>% filter(drug != "Semaglutide") %>% pull(drug),
    function(x) {
        c(paste0("Semaglutide_Population_for_Semaglutide_vs_", x), 
          paste0(x, "_Population_for_Semaglutide_vs_", x))
    }
))


cohorts_table <- dte_cohort_data.3 %>%
summarise(across(all_of(cohort_names), ~sum(.))) %>%
mutate(across(everything(), ~comma(.)))


colnames(cohorts_table) <- c(cohort_names %>% str_extract("^[^_]+_[^_]+") %>% str_replace_all("_", " "))

html_text <- cohorts_table %>%
kbl(escape = F, align = "c") %>%
kable_paper("hover", full_width = F) %>%
add_header_above(
    c(cohort_names %>% str_extract("[^_]+_[^_]+_[^_]+$") %>% str_replace_all("_", " ") %>% factor(levels = unique(.)) %>% table() %>% c)
) %>%
as.character 

html_text %>%
display_html
#webshot::webshot(html_filename, png_filename)

# %%
load("dte_cohort_Dispense_Data.rds")

# %%
dte_cohort_data.4 <- dte_cohort_data.2 %>%
left_join(dte_cohort_dispense_data, by = "person_id")

for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]

    if(this_drug == "Semaglutide"){next}

    var_name_meets_visit_criteria <- paste0(this_drug, "_meets_visit_criteria")
    var_name_meets_conf_criteria <- paste0(this_drug, "_was_confirmed")
    var_name_meets_disp_criteria <- paste0(this_drug, "_was_dispensed")
    var_name_semaglutide_pop <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    var_name_other_drug_pop <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    dte_cohort_data.4 <- dte_cohort_data.4 %>%
    mutate(!!sym(var_name_semaglutide_pop) := !!sym(var_name_semaglutide_pop) & Semaglutide_meets_visit_criteria & Semaglutide_was_confirmed & Semaglutide_was_dispensed,
           !!sym(var_name_other_drug_pop) := !!sym(var_name_other_drug_pop) & !!sym(var_name_meets_visit_criteria) & !!sym(var_name_meets_conf_criteria) & !!sym(var_name_meets_disp_criteria)
          )

}

# %%
cohort_names <- do.call(c, lapply(
    drug_timeline_info %>% filter(drug != "Semaglutide") %>% pull(drug),
    function(x) {
        c(paste0("Semaglutide_Population_for_Semaglutide_vs_", x), 
          paste0(x, "_Population_for_Semaglutide_vs_", x))
    }
))


cohorts_table <- dte_cohort_data.4 %>%
summarise(across(all_of(cohort_names), ~sum(.))) %>%
mutate(across(everything(), ~comma(.)))


colnames(cohorts_table) <- c(cohort_names %>% str_extract("^[^_]+_[^_]+") %>% str_replace_all("_", " "))

html_text <- cohorts_table %>%
kbl(escape = F, align = "c") %>%
kable_paper("hover", full_width = F) %>%
add_header_above(
    c(cohort_names %>% str_extract("[^_]+_[^_]+_[^_]+$") %>% str_replace_all("_", " ") %>% factor(levels = unique(.)) %>% table() %>% c)
) %>%
as.character 

html_text %>%
display_html

# %%
dte_cohort_data.4 <- dte_cohort_data.2 %>%
left_join(dte_cohort_dispense_data, by = "person_id")

for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]

    if(this_drug == "Semaglutide"){next}

    var_name_meets_visit_criteria <- paste0(this_drug, "_meets_visit_criteria")
    var_name_meets_conf_criteria <- paste0(this_drug, "_was_confirmed")
    var_name_semaglutide_pop <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    var_name_other_drug_pop <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    dte_cohort_data.4 <- dte_cohort_data.4 %>%
    mutate(!!sym(var_name_semaglutide_pop) := !!sym(var_name_semaglutide_pop) & Semaglutide_meets_visit_criteria & Semaglutide_was_confirmed,
           !!sym(var_name_other_drug_pop) := !!sym(var_name_other_drug_pop) & !!sym(var_name_meets_visit_criteria) & !!sym(var_name_meets_conf_criteria)
          )

}

# %%
cohort_names <- do.call(c, lapply(
    drug_timeline_info %>% filter(drug != "Semaglutide") %>% pull(drug),
    function(x) {
        c(paste0("Semaglutide_Population_for_Semaglutide_vs_", x), 
          paste0(x, "_Population_for_Semaglutide_vs_", x))
    }
))


cohorts_table <- dte_cohort_data.4 %>%
summarise(across(all_of(cohort_names), ~sum(.))) %>%
mutate(across(everything(), ~comma(.)))


colnames(cohorts_table) <- c(cohort_names %>% str_extract("^[^_]+_[^_]+") %>% str_replace_all("_", " "))

html_text <- cohorts_table %>%
kbl(escape = F, align = "c") %>%
kable_paper("hover", full_width = F) %>%
add_header_above(
    c(cohort_names %>% str_extract("[^_]+_[^_]+_[^_]+$") %>% str_replace_all("_", " ") %>% factor(levels = unique(.)) %>% table() %>% c)
) %>%
as.character 

html_text %>%
display_html

# %%
