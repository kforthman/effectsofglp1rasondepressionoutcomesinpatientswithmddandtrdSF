# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("data_prep_functions.txt")
notebook_setup()

# %%
antidepressant_drug_class <- read.csv("Drug_ATC_Categories.csv") %>% arrange(Name)
antidepressant_drug_class %>% head

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Antidepressants-Drug_Exposure.rds")
antidepressant_record <- data_prep
antidepressant_record %>% head

data.frame(Name = unique(antidepressant_record$concept_name_detail)) %>%
filter(grepl("/",Name)) %>%
rowwise() %>%
mutate(Category_Level_4 = paste(unique(antidepressant_drug_class$Category_Level_4[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_3 = paste(unique(antidepressant_drug_class$Category_Level_3[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_2 = paste(unique(antidepressant_drug_class$Category_Level_2[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_1 = paste(unique(antidepressant_drug_class$Category_Level_1[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(ATC_code =         paste(unique(        antidepressant_drug_class$ATC_code[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Antipsychotics-Drug_Exposure.rds")
drug_record <- data_prep
drug_record %>% head

data.frame(Name = unique(drug_record$concept_name_detail)) %>%
filter(grepl("/",Name)) %>%
rowwise() %>%
mutate(Category_Level_4 = paste(unique(antidepressant_drug_class$Category_Level_4[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_3 = paste(unique(antidepressant_drug_class$Category_Level_3[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_2 = paste(unique(antidepressant_drug_class$Category_Level_2[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_1 = paste(unique(antidepressant_drug_class$Category_Level_1[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(ATC_code =         paste(unique(        antidepressant_drug_class$ATC_code[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Anxiolytics-Drug_Exposure.rds")
drug_record <- data_prep
drug_record %>% head

data.frame(Name = unique(drug_record$concept_name_detail)) %>%
filter(grepl("/",Name)) %>%
rowwise() %>%
mutate(Category_Level_4 = paste(unique(antidepressant_drug_class$Category_Level_4[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_3 = paste(unique(antidepressant_drug_class$Category_Level_3[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_2 = paste(unique(antidepressant_drug_class$Category_Level_2[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_1 = paste(unique(antidepressant_drug_class$Category_Level_1[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(ATC_code =         paste(unique(        antidepressant_drug_class$ATC_code[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Hypnotics_and_Sedatives-Drug_Exposure.rds")
drug_record <- data_prep
drug_record %>% head

data.frame(Name = unique(drug_record$concept_name_detail)) %>%
filter(grepl("/",Name)) %>%
rowwise() %>%
mutate(Category_Level_4 = paste(unique(antidepressant_drug_class$Category_Level_4[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_3 = paste(unique(antidepressant_drug_class$Category_Level_3[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_2 = paste(unique(antidepressant_drug_class$Category_Level_2[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_1 = paste(unique(antidepressant_drug_class$Category_Level_1[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(ATC_code =         paste(unique(        antidepressant_drug_class$ATC_code[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Psychostimulants_Agents_used_for_ADHD_and_Nootropics-Drug_Exposure.rds")
drug_record <- data_prep
drug_record %>% head

data.frame(Name = unique(drug_record$concept_name_detail)) %>%
filter(grepl("/",Name)) %>%
rowwise() %>%
mutate(Category_Level_4 = paste(unique(antidepressant_drug_class$Category_Level_4[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_3 = paste(unique(antidepressant_drug_class$Category_Level_3[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_2 = paste(unique(antidepressant_drug_class$Category_Level_2[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_1 = paste(unique(antidepressant_drug_class$Category_Level_1[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(ATC_code =         paste(unique(        antidepressant_drug_class$ATC_code[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Psycholeptics_and_Psychoanaleptics_in_Combination-Drug_Exposure.rds")
drug_record <- data_prep
drug_record %>% head

data.frame(Name = unique(drug_record$concept_name_detail)) %>%
filter(grepl("/",Name)) %>%
rowwise() %>%
mutate(Category_Level_4 = paste(unique(antidepressant_drug_class$Category_Level_4[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_3 = paste(unique(antidepressant_drug_class$Category_Level_3[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_2 = paste(unique(antidepressant_drug_class$Category_Level_2[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_1 = paste(unique(antidepressant_drug_class$Category_Level_1[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(ATC_code =         paste(unique(        antidepressant_drug_class$ATC_code[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Anti_Dementia_Drugs-Drug_Exposure.rds")
drug_record <- data_prep
drug_record %>% head

data.frame(Name = unique(drug_record$concept_name_detail)) %>%
filter(grepl("/",Name)) %>%
rowwise() %>%
mutate(Category_Level_4 = paste(unique(antidepressant_drug_class$Category_Level_4[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_3 = paste(unique(antidepressant_drug_class$Category_Level_3[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_2 = paste(unique(antidepressant_drug_class$Category_Level_2[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(Category_Level_1 = paste(unique(antidepressant_drug_class$Category_Level_1[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
mutate(ATC_code =         paste(unique(        antidepressant_drug_class$ATC_code[antidepressant_drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))

# %%
