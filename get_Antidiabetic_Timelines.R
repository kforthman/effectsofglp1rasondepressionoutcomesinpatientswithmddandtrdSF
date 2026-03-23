# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("ehr_length_noObs_v2.rds")
ehr_length_tab <- ehr_length
#ehr_length_tab %>% head

# %%
all.data.1 <- ehr_length_tab %>% dplyr::select(person_id)

# %%
all_cohort_ids <- all.data.1

# %%
ehr_timeline <- ehr_length %>%
rename(ehr_start_datetime = first_visit, ehr_end_datetime = last_visit)
ehr_timeline %>% head

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Depression-Condition_Occurrence.rds")
mdd_record <- data_prep

mdd_first_record <- mdd_record %>% 
group_by(person_id) %>% 
filter(start_datetime == min(start_datetime)) %>% 
dplyr::select(person_id, start_datetime) %>%
rename(first_depression_dx_date = start_datetime) %>%
slice(1) %>%
ungroup()


mdd_timeline <- mdd_first_record
remove(mdd_record, mdd_first_record)

mdd_timeline %>% head

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
# Identify first drug record and the length of EHR record before and after.
get_first_drug_record <- function(drug_data, all_cohort_ids, ehr_timeline, mdd_timeline){
    drug_first_record <- drug_data %>% 
    dplyr::select(person_id, start_datetime, end_datetime) %>%
    filter(person_id %in% all_cohort_ids$person_id) %>%
    rename(drug_start_datetime = start_datetime, drug_end_datetime = end_datetime) %>%
    group_by(person_id) %>% 
    filter(drug_start_datetime == min(drug_start_datetime)) %>% 
    slice(1) %>%
    ungroup()

    drug_timeline <- drug_first_record %>%
    left_join(ehr_timeline, by = "person_id") %>%
    left_join(mdd_timeline, by = "person_id") %>%
    mutate(first_ehr_to_drug_months = time_length(interval(ehr_start_datetime, drug_start_datetime), "months"),
           drug_to_last_ehr_months = time_length(interval(drug_start_datetime, ehr_end_datetime), "months"),
           mdd_to_drug_months = time_length(interval(first_depression_dx_date, drug_start_datetime), "months"),
           mdd_to_drug_days = time_length(interval(first_depression_dx_date, drug_start_datetime), "days"),
           age_at_index_years = floor(time_length(interval(DOB, drug_start_datetime), "years"))
           )%>% 
    mutate(sufficient_prior = first_ehr_to_drug_months > 6,
           sufficient_post = drug_to_last_ehr_months > 12,
           mdd_before_drug = ifelse(is.na(mdd_to_drug_months > 0), FALSE, mdd_to_drug_months > 0),
           total_met = sufficient_prior + sufficient_post + mdd_before_drug
           )
    return(drug_timeline)
}

# Create a drug timeline 
get_drug_timeline <- function(drug_data, drug_name, all_cohort_ids, ehr_timeline, mdd_timeline){
    drug_record <- drug_data %>% 
    dplyr::select(person_id, start_datetime, end_datetime) %>%
    filter(person_id %in% all_cohort_ids$person_id) %>%
    mutate(drug = drug_name) %>%
    rename(drug_start_datetime = start_datetime, drug_end_datetime = end_datetime) %>%
    relocate(drug, .after = 1)
    
    return(drug_record)
}

# Find instances where comparitor antidiabetic overlaps 6m before or 12m after the index drug
define_overlap_ids <- function(index_drug_record, comparison_drug_timeline){
    this_group <- index_drug_record %>%
    pull(person_id)

    index_overlap_timeline <- comparison_drug_timeline %>% filter(person_id %in% this_group) %>%
    left_join(
        index_drug_record %>%
        rename(index_drug_start_datetime = drug_start_datetime) %>%
        dplyr::select(person_id, index_drug_start_datetime),
        by = "person_id"
    ) %>%
    mutate(time_from_index_months = time_length(interval(index_drug_start_datetime, 
                                                         drug_start_datetime), 
                                                "months")) %>%
    filter(time_from_index_months > -6 & time_from_index_months < 12) %>%
    arrange(person_id, drug_start_datetime)

    index_overlap_id <- index_overlap_timeline %>% dplyr::select(person_id) %>% group_by(person_id) %>% slice(1) %>% ungroup

    return(index_overlap_id)
}

# %%
# Create the drug data frames
for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]
    
    var_name <- paste0(this_drug, "_data")
    var_name_timeline <- paste0(this_drug, "_timeline")
    var_name_first_drug_record <- paste0(this_drug, "_first_drug_record")
    
    
    this_timeline <- get_drug_timeline(get(var_name), this_drug, all_cohort_ids, ehr_timeline, mdd_timeline)
    this_first_drug_record <- get_first_drug_record(get(var_name), all_cohort_ids, ehr_timeline, mdd_timeline)
    
    assign(var_name_timeline, this_timeline)
    assign(var_name_first_drug_record, this_first_drug_record) 
    
    save(this_timeline, file = paste0(var_name_timeline, ".rds"))
    save(this_first_drug_record, file = paste0(var_name_first_drug_record, ".rds"))
}

# %%
# Create columns to identify overlapping drugs and identify possible drug cohorts
all.data.2 <- all.data.1

for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]
    
    var_name_timeline <- paste0(this_drug, "_timeline")
    var_name_first_drug_record <- paste0(this_drug, "_first_drug_record")
    var_name_age_at_index_years <- paste0(this_drug, "_age_at_index_years")
    var_name_mdd_to_index_days <- paste0(this_drug, "_mdd_to_index_days")
    var_name_Meets_Timeline_Criteria <- paste0(this_drug, "_Meets_Timeline_Criteria")
    
    these_ids <- get(var_name_first_drug_record) %>% filter(sufficient_prior & sufficient_post & mdd_before_drug) %>% pull(person_id)
    
    drug_start_table <- get(var_name_first_drug_record) %>% 
    dplyr::select(person_id, drug_start_datetime, age_at_index_years, mdd_to_drug_days) %>% 
    rename(!!sym(var_name_first_drug_record) := drug_start_datetime, 
           !!sym(var_name_age_at_index_years) := age_at_index_years,
           !!sym(var_name_mdd_to_index_days) := mdd_to_drug_days)
    
    all.data.2 <- all.data.2 %>%
    mutate(!!sym(var_name_Meets_Timeline_Criteria) := person_id %in% these_ids) %>%
    left_join(drug_start_table, by = join_by(person_id))
    
    for(j in 1:nrow(drug_timeline_info)){
        this_other_drug <- drug_timeline_info$drug[j]
        if(this_drug == this_other_drug){next}
        
        var_name_other_timeline <- paste0(this_other_drug, "_timeline")
        
        var_name_other_overlaps_this_index <- paste0(this_other_drug, "_Overlaps_", this_drug, "_Index")
        these_ids_other_overlaps_this_index <- define_overlap_ids(get(var_name_first_drug_record), get(var_name_other_timeline)) %>% pull(person_id)
        
        all.data.2 <- all.data.2 %>%
        mutate(!!sym(var_name_other_overlaps_this_index) := person_id %in% these_ids_other_overlaps_this_index)
    }
    
    if(this_drug == "Semaglutide"){next}
    
    var_name_other_overlaps_semaglutide_index <- paste0(this_drug, "_Overlaps_Semaglutide_Index")
    var_name_semaglutide_overlaps_other_index <- paste0("Semaglutide_Overlaps_", this_drug, "_Index")
    var_name_semaglutide_pop <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    var_name_other_drug_pop <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)
    
    all.data.2 <- all.data.2 %>%
    mutate(!!sym(var_name_semaglutide_pop) := Semaglutide_Meets_Timeline_Criteria & !(!!sym(var_name_other_overlaps_semaglutide_index)),
           !!sym(var_name_other_drug_pop) := !!sym(var_name_Meets_Timeline_Criteria) & !(!!sym(var_name_semaglutide_overlaps_other_index)) & !(!!sym(var_name_semaglutide_pop))
          )
    
    }

# %%
this.data <- all.data.2

# %%
this.data %>% dim
this.data %>% head %>% print_all_cols

# %%
save(this.data, file = "data_DTE_AntidiabeticTimelineVars.rds")

# %% [markdown]
# Summary

# %%
this.data %>% filter(!is.na(Semaglutide_first_drug_record)) %>% head(10)

# %%
range(this.data %>% filter(!is.na(Semaglutide_age_at_index_years)) %>% pull(Semaglutide_age_at_index_years))
range(this.data %>% filter(!is.na(Insulins_age_at_index_years)) %>% pull(Insulins_age_at_index_years))
range(this.data %>% filter(!is.na(Metformin_age_at_index_years)) %>% pull(Metformin_age_at_index_years))
range(this.data %>% filter(!is.na(DPP4i_age_at_index_years)) %>% pull(DPP4i_age_at_index_years))
range(this.data %>% filter(!is.na(SGLT2i_age_at_index_years)) %>% pull(SGLT2i_age_at_index_years))
range(this.data %>% filter(!is.na(SU_age_at_index_years)) %>% pull(SU_age_at_index_years))
range(this.data %>% filter(!is.na(TZD_age_at_index_years)) %>% pull(TZD_age_at_index_years))
range(this.data %>% filter(!is.na(GLP1RA_age_at_index_years)) %>% pull(GLP1RA_age_at_index_years))

# %%
hist(this.data %>% filter(!is.na(Semaglutide_age_at_index_years)) %>% pull(Semaglutide_age_at_index_years))
hist(this.data %>% filter(!is.na(Insulins_age_at_index_years)) %>% pull(Insulins_age_at_index_years))
hist(this.data %>% filter(!is.na(Metformin_age_at_index_years)) %>% pull(Metformin_age_at_index_years))
hist(this.data %>% filter(!is.na(DPP4i_age_at_index_years)) %>% pull(DPP4i_age_at_index_years))
hist(this.data %>% filter(!is.na(SGLT2i_age_at_index_years)) %>% pull(SGLT2i_age_at_index_years))
hist(this.data %>% filter(!is.na(SU_age_at_index_years)) %>% pull(SU_age_at_index_years))
hist(this.data %>% filter(!is.na(TZD_age_at_index_years)) %>% pull(TZD_age_at_index_years))
hist(this.data %>% filter(!is.na(GLP1RA_age_at_index_years)) %>% pull(GLP1RA_age_at_index_years))
