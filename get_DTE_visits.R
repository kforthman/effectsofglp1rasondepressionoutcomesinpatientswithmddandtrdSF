# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("dte_cohort_wNontreat_data.rds", verbose = T)
dte_cohort_data <- this.data

dte_cohort_data %>% head(10)

# %%
load("Data_Prepped/Prepped_Data-All_Participants-All_Visits-Visit.rds", verbose = T)
dim(data_prep) %>% comma
visit_tiers <- read.csv("visit_type_tiers-manualEdit-MPP.csv")
visit_data <- data_prep %>% 
filter(person_id %in% this.data$person_id) %>%
left_join(
    visit_tiers %>% dplyr::select(standard_concept_id, acuity) %>% mutate(standard_concept_id = as.character(standard_concept_id)), 
          by = join_by(concept_id == standard_concept_id)
)

save(visit_data, file = "dte_cohort_visits.rds")

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Psychologic_or_Psychiatric_Visits_v2-Combined.rds", verbose = T)
data_prep %>% 
dplyr::select(source_concept_id, source_concept_code, source_vocabulary, source_concept_name) %>%
count(source_concept_id, source_concept_code, source_vocabulary, source_concept_name) %>%
rename(count_records = "n") %>%
write.csv("psych_proc_tiers.csv", row.names = F)

# %%
dim(data_prep) %>% comma

psych_proc_tiers <- read.csv("psych_proc_tiers-manualEdit-MPP.csv")

psych_visit_data <- data_prep %>% 
filter(person_id %in% this.data$person_id) %>%
left_join(
    psych_proc_tiers %>% dplyr::select(source_concept_code, level) %>% mutate(source_concept_code = as.character(source_concept_code)), 
          by = "source_concept_code"
)


dim(psych_visit_data) %>% comma

save(psych_visit_data, file = "dte_cohort_psych_visits.rds")

# %%
