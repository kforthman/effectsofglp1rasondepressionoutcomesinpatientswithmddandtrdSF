# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

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
load("dte_cohort_data.rds")
dte_cohort_data <- this.data

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
for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]
    
    var_name <- paste0(this_drug, "_data")
    this_data <- get(var_name) %>%
    filter(person_id %in% dte_cohort_data$person_id)
    these_drug_ids <- this_data %>% dplyr::select(person_id) %>% group_by(person_id) %>% slice(1) %>% ungroup
    
    flush.console()
    cat(paste0("\n\n", this_drug, " (n = ", comma(nrow(these_drug_ids)), ")\n\n"))

    this_index_period <- this_data  %>%
    group_by(person_id) %>% 
    mutate(drug_index_datetime = min(start_datetime)) %>%
    ungroup %>%
    mutate(time_from_index = time_length(interval(drug_index_datetime, start_datetime), "days")) %>%
    filter(time_from_index <= 30)
    
    total_n <- this_index_period %>% group_by(person_id) %>% summarize(total_records_in_30_days = n())
    
    total_dispensed <- this_index_period %>%
    filter(time_from_index <= 7) %>% 
    filter(type_concept_name %in% c("Prescription dispensed in pharmacy", "EHR dispensing record", "Dispensed in Outpatient office")) %>% 
    group_by(person_id) %>%
    slice(1)
    
    var_name <- paste0(this_drug, "_index_info")
    paste0("\nCreating variable ", var_name, "\n") %>% cat
    assign(var_name, this_index_period)
    
    numerator <- sum(total_n$total_records_in_30_days > 1)
    denominator <- nrow(these_drug_ids)
    paste0("\n Total individuals with confirmatory med records within 30 days of index: ", comma(numerator), " / ", 
           comma(denominator), 
           "  (", percent(numerator/denominator), ")") %>% cat
    
    numerator <- nrow(total_dispensed)
    denominator <- nrow(these_drug_ids)
    paste0("\n Total individuals with dispense records within 7 days of index: ", comma(numerator), " / ", 
           comma(denominator), 
           "  (", percent(numerator/denominator), ")") %>% cat
    cat("\n--------------------------------------------------------------\n\n")
}
    

# %%
rbind(
   Semaglutide_index_info,
    Insulins_index_info,
    Metformin_index_info,
    DPP4i_index_info,
    SGLT2i_index_info,
    SU_index_info,
    TZD_index_info,
    GLP1RA_index_info
    ) %>%
filter(time_from_index == 0) %>%
count(type_concept_name) %>%
arrange(-n) %>%
mutate(n = comma(n))

# %%
rbind(
   Semaglutide_index_info,
    Insulins_index_info,
    Metformin_index_info,
    DPP4i_index_info,
    SGLT2i_index_info,
    SU_index_info,
    TZD_index_info,
    GLP1RA_index_info
    ) %>%
count(type_concept_name) %>%
arrange(-n) %>%
mutate(n = comma(n))

# %%
rbind(
   Semaglutide_index_info,
    Insulins_index_info,
    Metformin_index_info,
    DPP4i_index_info,
    SGLT2i_index_info,
    SU_index_info,
    TZD_index_info,
    GLP1RA_index_info
    ) %>%
group_by(person_id, concept_name) %>% 
mutate(total_records_in_30_days = n()) %>%
ungroup() %>%
filter(total_records_in_30_days == 1) %>%
count(type_concept_name) %>%
arrange(-n) %>%
mutate(n = comma(n))

# %%
dte_cohort_data.2 <- dte_cohort_data

for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]
    
    var_name <- paste0(this_drug, "_data")
    this_data <- get(var_name)

    this_index_period <- this_data %>% 
    group_by(person_id) %>% 
    mutate(drug_index_datetime = min(start_datetime)) %>%
    ungroup %>%
    mutate(time_from_index = time_length(interval(drug_index_datetime, start_datetime), "days")) %>%
    filter(time_from_index <= 30)
    
    was_confirmed <- this_index_period %>% 
    group_by(person_id) %>% 
    mutate(total_records_in_30_days = n()) %>%
    filter(total_records_in_30_days > 1) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(person_id)
    
    was_dispensed <- this_index_period %>%
    filter(time_from_index <= 7) %>% 
    filter(type_concept_name %in% c("Prescription dispensed in pharmacy", "EHR dispensing record", "Dispensed in Outpatient office")) %>% 
    group_by(person_id) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(person_id)
    
    dte_cohort_data.2 <- dte_cohort_data.2 %>%
    mutate(!!sym(paste0(this_drug, "_was_confirmed")) := person_id %in% was_confirmed$person_id) %>%
    mutate(!!sym(paste0(this_drug, "_was_dispensed")) := person_id %in% was_dispensed$person_id)
    
}

dte_cohort_data.2 %>% head

# %%
dte_cohort_dispense_data <- dte_cohort_data.2 %>% dplyr::select(person_id, ends_with("_was_confirmed"), ends_with("_was_dispensed"))
save(dte_cohort_dispense_data, file = "dte_cohort_Dispense_Data.rds")

# %%
dte_cohort_data.3 <- dte_cohort_data.2

for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]

    if(this_drug == "Semaglutide"){next}

    var_name_meets_conf_criteria <- paste0(this_drug, "_was_confirmed")
    var_name_meets_disp_criteria <- paste0(this_drug, "_was_dispensed")
    var_name_semaglutide_pop <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    var_name_other_drug_pop <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    dte_cohort_data.3 <- dte_cohort_data.3 %>%
    mutate(!!sym(var_name_semaglutide_pop) := !!sym(var_name_semaglutide_pop) & Semaglutide_was_confirmed & Semaglutide_was_dispensed,
           !!sym(var_name_other_drug_pop) := !!sym(var_name_other_drug_pop) & !!sym(var_name_meets_conf_criteria) & !!sym(var_name_meets_disp_criteria)
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

# %%
dte_cohort_data.3 <- dte_cohort_data.2

for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]

    if(this_drug == "Semaglutide"){next}

    var_name_meets_conf_criteria <- paste0(this_drug, "_was_confirmed")
    var_name_semaglutide_pop <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    var_name_other_drug_pop <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    dte_cohort_data.3 <- dte_cohort_data.3 %>%
    mutate(!!sym(var_name_semaglutide_pop) := !!sym(var_name_semaglutide_pop) & Semaglutide_was_confirmed,
           !!sym(var_name_other_drug_pop) := !!sym(var_name_other_drug_pop) & !!sym(var_name_meets_conf_criteria)
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

# %%
