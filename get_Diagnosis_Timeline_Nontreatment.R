# %% [markdown]
# pre-requisites:
# - get_Antidiabetic_Nontreatment_Timelines_v3.R

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
# Load the Nontreatment index dates
load("dte_cohort_wNontreat_data.rds")
dte_cohort_wNontreat_data <- this.data

nontreatment_index_record <- dte_cohort_wNontreat_data %>%
filter(Nontreatment_Population_for_Semaglutide_vs_Nontreatment == TRUE) %>%
dplyr::select(person_id, Nontreatment_index) %>%
mutate(Nontreatment_index = as.POSIXct(Nontreatment_index))

nontreatment_index_record %>% head

# %%
# Loop over each diagnosis file path
for(i in 1:nrow(diagnoses_timeline_info)){
    paste0("Loading file \"", diagnoses_timeline_info$file_path[i], "\"\n") %>% cat
    load(diagnoses_timeline_info$file_path[i])

    var_name <- paste0(diagnoses_timeline_info$diagnosis[i], "_data")
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, data_prep)
}

# %%
define_before_diagnosis_ids_nontreatment <- function(index_record, diagnosis_timeline){
    this_group <- index_record %>%
    pull(person_id)

    index_before_timeline <- diagnosis_timeline %>% filter(person_id %in% this_group) %>%
    left_join(
        index_record %>%
        rename(index_start_datetime = Nontreatment_index) %>%
        dplyr::select(person_id, index_start_datetime),
        by = "person_id"
    ) %>%
    mutate(time_from_index_months = time_length(interval(index_start_datetime,
                                                         start_datetime),
                                                "months")) %>%
    filter(time_from_index_months <= 0) %>%
    arrange(person_id, start_datetime)

    index_before_id <- index_before_timeline %>% dplyr::select(person_id) %>% group_by(person_id) %>% slice(1) %>% ungroup

    return(index_before_id)
}

# %%
define_overlap_diagnosis_ids_nontreatment <- function(index_record, diagnosis_timeline){
    this_group <- index_record %>%
    pull(person_id)

    index_overlap_timeline <- diagnosis_timeline %>% filter(person_id %in% this_group) %>%
    left_join(
        index_record %>%
        rename(index_start_datetime = Nontreatment_index) %>%
        dplyr::select(person_id, index_start_datetime),
        by = "person_id"
    ) %>%
    mutate(time_from_index_months = time_length(interval(index_start_datetime,
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
    this_overlap <- define_overlap_diagnosis_ids_nontreatment(nontreatment_index_record, this_diagnosis_data) %>% pull(person_id)
    all.data.2 <- all.data.2 %>% mutate(!!sym(paste0(this_diagnosis, "_Overlaps_Nontreatment_Index")) := person_id %in% this_overlap)
}

# %%
all.data.3 <- all.data.2

for(i in 1:nrow(diagnoses_timeline_info)){
    this_diagnosis <- diagnoses_timeline_info$diagnosis[i]
    this_diagnosis_data <- get(paste0(this_diagnosis, "_data"))
    this_before <- define_before_diagnosis_ids_nontreatment(nontreatment_index_record, this_diagnosis_data) %>% pull(person_id)
    all.data.3 <- all.data.3 %>% mutate(!!sym(paste0(this_diagnosis, "_Before_Nontreatment_Index")) := person_id %in% this_before)
}

# %%
this.data <- all.data.3

# %%
this.data %>% dim
this.data %>% head %>% print_all_cols

# %%
save(this.data, file = "data_DTE_DiagnosisTimelineVars_Nontreatment.rds")

# %%
