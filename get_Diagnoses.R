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
diagnoses_info <- c("T2DM", "Data_Prepped/IDs-Type_2_Diabetes_Mellitus-Condition_Occurrence.txt",
               "MDD", "Data_Prepped/IDs-Depression-Condition_Occurrence.txt",
               "TRD", "Data_Prepped/IDs-TRD.txt",
               "BD", "Data_Prepped/IDs-Bipolar_Disorder-Condition_Occurrence.txt",
               "SCH", "Data_Prepped/IDs-Schizophrenia-Condition_Occurrence.txt",
               "Obese", "Data_Prepped/IDs-Obesity-Combined.txt",
               "Hypertension", "Data_Prepped/IDs-Hypertension-Condition_Occurrence.txt",
               "Hypercholesterolemia", "Data_Prepped/IDs-Hypercholesterolemia-Condition_Occurrence.txt",
               "Hyperlipidemia", "Data_Prepped/IDs-Hyperlipidemia-Condition_Occurrence.txt",
               "Heart_Disease", "Data_Prepped/IDs-Heart_Disease-Condition_Occurrence.txt",
               "Stroke", "Data_Prepped/IDs-Stroke-Combined.txt",
               "Chronic_Kidney_Disease", "Data_Prepped/IDs-Chronic_Kidney_Disease-Condition_Occurrence.txt",
               "A1C_over_8p5", "Data_Prepped/IDs-A1C_over_8p5-Measurement.txt",
               "Pancreatitis", "Data_Prepped/IDs-Pancreatitis-Condition_Occurrence.txt",
               "T1DM", "Data_Prepped/IDs-Type_1_Diabetes-Condition_Occurrence.txt",
               "Thyroid_Cancer", "Data_Prepped/IDs-Thyroid_Cancer-Combined.txt",
               "Gastroparesis", "Data_Prepped/IDs-Gastroparesis-Condition_Occurrence.txt",
                    
               "Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances", "Data_Prepped/IDs-Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances-Combined.txt",  
               "Problems_Related_to_Lifestyle", "Data_Prepped/IDs-Problems_Related_to_Lifestyle-Combined.txt", 
               "Type_2_Diabetes_Mellitus_with_Complications", "Data_Prepped/IDs-Type_2_Diabetes_Mellitus_with_Complications-Condition_Occurrence.txt",
               "Bariatric_Surgery", "Data_Prepped/IDs-Bariatric_Surgery_Status-Combined.txt",
               "Diseases_of_the_Arteries_Artrioles_and_Capillaries", "Data_Prepped/IDs-Diseases_of_the_Arteries_Artrioles_and_Capillaries-Condition_Occurrence.txt", 
                    

               "ADHD", "Data_Prepped/IDs-ADHD-Condition_Occurrence.txt", 
               "Agoraphobia", "Data_Prepped/IDs-Agoraphobia-Condition_Occurrence.txt", 
               "Anxiety_Disorder_NOS", "Data_Prepped/IDs-Anxiety_Disorder_NOS-Condition_Occurrence.txt", 
               "Generalized_Anxiety", "Data_Prepped/IDs-Generalized_Anxiety-Condition_Occurrence.txt", 
               "OCD", "Data_Prepped/IDs-OCD-Condition_Occurrence.txt", 
               "Panic_Disorder", "Data_Prepped/IDs-Panic_Disorder-Condition_Occurrence.txt", 
               "PTSD", "Data_Prepped/IDs-PTSD-Condition_Occurrence.txt", 
               "Social_Anxiety_Disorder", "Data_Prepped/IDs-Social_Anxiety_Disorder-Condition_Occurrence.txt", 
                    
                    
               "Alcohol_Abuse", "Data_Prepped/IDs-Alcohol_Abuse-Combined.txt", 
               "Alcohol_Dependence", "Data_Prepped/IDs-Alcohol_Dependence-Combined.txt", 
               "Cannabis_Abuse", "Data_Prepped/IDs-Cannabis_Abuse-Condition_Occurrence.txt", 
               "Cannabis_Dependence", "Data_Prepped/IDs-Cannabis_Dependence-Condition_Occurrence.txt", 
               "Cocaine_Abuse", "Data_Prepped/IDs-Cocaine_Abuse-Condition_Occurrence.txt", 
               "Cocaine_Dependence", "Data_Prepped/IDs-Cocaine_Dependence-Condition_Occurrence.txt", 
               "Opioid_Abuse", "Data_Prepped/IDs-Opioid_Abuse-Condition_Occurrence.txt", 
               "Opioid_Dependence", "Data_Prepped/IDs-Opioid_Dependence-Condition_Occurrence.txt", 
               "Sedative_Abuse", "Data_Prepped/IDs-Sedative_Abuse-Condition_Occurrence.txt", 
               "Sedative_Dependence", "Data_Prepped/IDs-Sedative_Dependence-Condition_Occurrence.txt",
               "Tobacco_Use_Disorder", "Data_Prepped/IDs-Tobacco_Use_Disorder-Combined.txt",
                    
               "Antidepressants", "Data_Prepped/IDs-Antidepressants-Drug_Exposure.txt"
              ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("diagnosis", "file_path"))

diagnoses_info

# %%
all.data.2 <- all.data.1

# Read in the id files and add binary variable to all.data
for(i in 1:nrow(diagnoses_info)){
    this_data <- read.csv(diagnoses_info$file_path[i], header = F) %>% 
    rename(person_id = V1) %>% mutate(!!sym(diagnoses_info$diagnosis[i]) := 1) %>% 
    mutate(person_id = as.character(person_id))
    
    all.data.2 <- all.data.2 %>%
    left_join(this_data, by = "person_id") %>%
    mutate(!!sym(diagnoses_info$diagnosis[i]) := replace(!!sym(diagnoses_info$diagnosis[i]), is.na(!!sym(diagnoses_info$diagnosis[i])), 0)) %>%
    mutate(!!sym(diagnoses_info$diagnosis[i]) := as.logical(!!sym(diagnoses_info$diagnosis[i])))
    
    var_name <- paste0(diagnoses_info$diagnosis[i], "_id_data")
    
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, this_data)
}

# %%
all.data.3 <- all.data.2

all.data.3 <- all.data.3 %>%
mutate(MDD_noBDnoSCH = replace(MDD, BD+SCH>0, FALSE)) %>%
mutate(TRD_noBDnoSCH = replace(TRD, BD+SCH>0, FALSE)) %>%
mutate(Meets_Wang_Criteria = 
       T2DM & 
       MDD_noBDnoSCH & 
       (Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5) &
       !(Pancreatitis | T1DM | Thyroid_Cancer | Gastroparesis)
       ) %>%
mutate(Meets_Custom_Eligibility_Requirements = 
       MDD_noBDnoSCH & 
       (T2DM | Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5) &
       !(Pancreatitis | Thyroid_Cancer | Gastroparesis)
      )

# %%
this.data <- all.data.3

# %%
this.data %>% dim
this.data %>% head %>% print_all_cols

# %%
save(this.data, file = "data_DTE_diagnoses.rds")

# %%
