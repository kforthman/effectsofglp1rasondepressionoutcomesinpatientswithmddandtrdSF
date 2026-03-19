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
all.data.1 <- ehr_length_tab %>% dplyr::select(person_id, ehr_length)

# %%
visits_info <- c(
"Total_All_Visits", "Data_Prepped/Total-All_Participants-All_Visits-Visit.rds", 
"Total_Outpatient_Visits", "Data_Prepped/Total-All_Participants-Outpatient_Visits-Visit.rds", 
"Total_Psychologic_or_Psychiatric_Procedure_or_Service", "Data_Prepped/Total-All_Participants-Psychologic_or_Psychiatric_Procedure_or_Service-Procedure_Occurrence.rds"
) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("visit_type", "file_path"))

visits_info

# %%
all.data.2 <- all.data.1

for(i in 1:nrow(visits_info)){
    load(visits_info$file_path[i])
    this_data <- data_prep %>% 
    rename(!!sym(visits_info$visit_type[i]) := n)
    
    all.data.2 <- all.data.2 %>%
    left_join(this_data, by = "person_id") %>%
    mutate(!!sym(visits_info$visit_type[i]) := replace(!!sym(visits_info$visit_type[i]), is.na(!!sym(visits_info$visit_type[i])), 0)) %>%
    mutate(!!sym(paste0("Average_", visits_info$visit_type[i], "_PerYear")) := !!sym(visits_info$visit_type[i])/ehr_length)
    
    paste0("Creating variable ", visits_info$visit_type[i], "\n") %>% cat
    paste0("Creating variable ", paste0("Average_", visits_info$visit_type[i], "_PerYear"), "\n") %>% cat
}

# %%
all.data.3 <- all.data.2 %>% dplyr::select(-ehr_length)

# %%
this.data <- all.data.3

# %%
this.data %>% dim
this.data %>% head %>% print_all_cols

# %%
save(this.data, file = "data_DTE_visitVars.rds")

# %%
