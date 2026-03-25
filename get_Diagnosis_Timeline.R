# %% [markdown]
# pre-requisites:
# - get_Antidiabetic_Timelines.ipynb

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
load("ehr_length_noObs_v2.rds")
ehr_length_tab <- ehr_length
#ehr_length_tab %>% head

# %%
all.data.1 <- ehr_length_tab %>% dplyr::select(person_id)

# %%
diagnoses_timeline_info <- c("T2DM", "Data_Prepped/Prepped_Data-All_Participants-Type_2_Diabetes_Mellitus-Condition_Occurrence.rds",
               "Obese", "Data_Prepped/Prepped_Data-All_Participants-Obesity-Combined.rds",
               "Hypertension", "Data_Prepped/Prepped_Data-All_Participants-Hypertension-Condition_Occurrence.rds",
               "Hypercholesterolemia", "Data_Prepped/Prepped_Data-All_Participants-Hypercholesterolemia-Condition_Occurrence.rds",
               "Hyperlipidemia", "Data_Prepped/Prepped_Data-All_Participants-Hyperlipidemia-Condition_Occurrence.rds",
               "Heart_Disease", "Data_Prepped/Prepped_Data-All_Participants-Heart_Disease-Condition_Occurrence.rds",
               "Stroke", "Data_Prepped/Prepped_Data-All_Participants-Stroke-Combined.rds",
               "Chronic_Kidney_Disease", "Data_Prepped/Prepped_Data-All_Participants-Chronic_Kidney_Disease-Condition_Occurrence.rds",
               "A1C_over_8p5", "Data_Prepped/Prepped_Data-All_Participants-A1C_over_8p5-Measurement.rds",
               "Pancreatitis", "Data_Prepped/Prepped_Data-All_Participants-Pancreatitis-Condition_Occurrence.rds",
               "T1DM", "Data_Prepped/Prepped_Data-All_Participants-Type_1_Diabetes-Condition_Occurrence.rds",
               "Thyroid_Cancer", "Data_Prepped/Prepped_Data-All_Participants-Thyroid_Cancer-Combined.rds",
               "Gastroparesis","Data_Prepped/Prepped_Data-All_Participants-Gastroparesis-Condition_Occurrence.rds"
              ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("diagnosis", "file_path"))

diagnoses_timeline_info

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
for(i in 1:nrow(diagnoses_timeline_info)){
    paste0("Loading file \"", diagnoses_timeline_info$file_path[i], "\"\n") %>% cat
    load(diagnoses_timeline_info$file_path[i])
    
    var_name <- paste0(diagnoses_timeline_info$diagnosis[i], "_data")
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, data_prep)
}

# %%
# Load the drug data frames
for(i in 1:nrow(drug_timeline_info)){
    this_drug <-  drug_timeline_info$drug[i]
    
    var_name <- paste0(this_drug, "_data")
    var_name_timeline <- paste0(this_drug, "_timeline")
    var_name_first_drug_record <- paste0(this_drug, "_first_drug_record")
    
    paste0("Loading file \"", paste0(var_name_timeline, ".rds"), "\"\n") %>% cat
    load(paste0(var_name_timeline, ".rds"))
    
    paste0("Creating variable ", var_name_timeline, "\n") %>% cat
    assign(var_name_timeline, this_timeline)
    
    paste0("Loading file \"", paste0(var_name_first_drug_record, ".rds"), "\"\n") %>% cat
    load(paste0(var_name_first_drug_record, ".rds"))
    
    paste0("Creating variable ", var_name_first_drug_record, "\n") %>% cat
    assign(var_name_first_drug_record, this_first_drug_record)
}

# %%
define_before_diagnosis_ids <- function(index_drug_record, diagnosis_timeline){
    this_group <- index_drug_record %>%
    pull(person_id)

    index_before_timeline <- diagnosis_timeline %>% filter(person_id %in% this_group) %>%
    left_join(
        index_drug_record %>%
        rename(index_drug_start_datetime = drug_start_datetime) %>%
        dplyr::select(person_id, index_drug_start_datetime),
        by = "person_id"
    ) %>%
    mutate(time_from_index_months = time_length(interval(index_drug_start_datetime,
                                                         start_datetime),
                                                "months")) %>%
    filter(time_from_index_months <= 0) %>%
    arrange(person_id, start_datetime)

    index_before_id <- index_before_timeline %>% dplyr::select(person_id) %>% group_by(person_id) %>% slice(1) %>% ungroup

    return(index_before_id)
}

# %%
define_overlap_diagnosis_ids <- function(index_drug_record, diagnosis_timeline){
    this_group <- index_drug_record %>%
    pull(person_id)

    index_overlap_timeline <- diagnosis_timeline %>% filter(person_id %in% this_group) %>%
    left_join(
        index_drug_record %>%
        rename(index_drug_start_datetime = drug_start_datetime) %>%
        dplyr::select(person_id, index_drug_start_datetime),
        by = "person_id"
    ) %>%
    mutate(time_from_index_months = time_length(interval(index_drug_start_datetime, 
                                                         start_datetime), 
                                                "months")) %>%
    filter(time_from_index_months > -6 & time_from_index_months < 12) %>%
    arrange(person_id, start_datetime)

    index_overlap_id <- index_overlap_timeline %>% dplyr::select(person_id) %>% group_by(person_id) %>% slice(1) %>% ungroup

    return(index_overlap_id)
}

# %%
all.data.2 <- all.data.1

for(i in 1:nrow(diagnoses_timeline_info)){
    this_diagnosis <- diagnoses_timeline_info$diagnosis[i]
    this_diagnosis_data <- get(paste0(this_diagnosis, "_data"))
    for(j in 1:nrow(drug_timeline_info)){
        this_drug <- drug_timeline_info$drug[j]
        this_index_drug_data <- get(paste0(this_drug, "_first_drug_record"))
        this_overlap <- define_overlap_diagnosis_ids(this_index_drug_data, this_diagnosis_data) %>% pull(person_id)
        all.data.2 <- all.data.2 %>% mutate(!!sym(paste0(this_diagnosis, "_Overlaps_", this_drug, "_Index")) := person_id %in% this_overlap)
        }
    }

# %%
all.data.3 <- all.data.2

for(i in 1:nrow(diagnoses_timeline_info)){
    this_diagnosis <- diagnoses_timeline_info$diagnosis[i]
    this_diagnosis_data <- get(paste0(this_diagnosis, "_data"))
    for(j in 1:nrow(drug_timeline_info)){
        this_drug <- drug_timeline_info$drug[j]
        this_index_drug_data <- get(paste0(this_drug, "_first_drug_record"))
        this_before <- define_before_diagnosis_ids(this_index_drug_data, this_diagnosis_data) %>% pull(person_id)
        all.data.3 <- all.data.3 %>% mutate(!!sym(paste0(this_diagnosis, "_Before_", this_drug, "_Index")) := person_id %in% this_before)
        }
    }

# %%
this.data <- all.data.3

# %%
this.data %>% dim
this.data %>% head %>% print_all_cols

# %%
save(this.data, file = "data_DTE_DiagnosisTimelineVars.rds")

# %%
