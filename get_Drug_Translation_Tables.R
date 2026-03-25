# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
translate_table <- function(concept_name, table_type, verbose = TRUE, drug_category_file = NULL, 
                            translation_table_unit = NULL, dose_override_table = NULL,
                            drug_category_filter_to = NULL, drug_specific_exclusions = NULL){

    load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)

    trans_tab_file <- str_glue("Data_Prepped/Translation_Table-{concept_name}.csv")

    if(!file.exists(trans_tab_file)){
        stop(str_glue("Translation table file does not exist. {trans_tab_file}"))
    }

    trans_tab <- read.csv(str_glue("Data_Prepped/Translation_Table-{concept_name}.csv"))

    data_prep <- data_prep %>%
    mutate(
        source_concept_name_split = as.character(source_concept_name)
    ) %>%
    separate_rows(
        source_concept_name_split,
        sep = " / "
    ) %>%
    mutate(
        source_concept_name_split = str_squish(source_concept_name_split)   # clean up extra whitespace
    )%>%
    relocate(source_concept_name_split, .before = 7) %>% 
    mutate(concept_name_detail = NA)

    for(i in 1:nrow(trans_tab)){
        data_prep <- data_prep %>%
        mutate(concept_name_detail = ifelse(grepl(trans_tab$str_search[i], source_concept_name_split, ignore.case = T, perl = TRUE) & is.na(concept_name_detail), 
                                            trans_tab$translation[i], 
                                            concept_name_detail))
    }


    data_prep <- data_prep %>%
    relocate(concept_name_detail, .before = 8)

    if(!is.null(drug_category_file)){

        drug_class <- read.csv("Drug_ATC_Categories.csv") %>% arrange(Name)
        data_prep <- data_prep %>% 
        left_join(drug_class, by =  join_by(concept_name_detail == Name)) %>%
        rename(concept_category_level_4 = Category_Level_4,
               concept_category_level_3 = Category_Level_3,
               concept_category_level_2 = Category_Level_2,
               concept_category_level_1 = Category_Level_1,
               concept_atc_code = ATC_code
              )

        if(!is.null(drug_category_filter_to)){
            exclude_drug <- data_prep %>%
            filter(!grepl(paste(drug_category_filter_to,collapse = "|"), concept_category_level_3)) %>% 
            pull(concept_name_detail) %>% 
            unique %>% sort

            data_prep <- data_prep %>% filter(!(concept_name_detail %in% exclude_drug))
        }
    }

    if(!is.null(translation_table_unit)){
        
        if(!is.null(dose_override_table)){
            
            data_prep <- data_prep %>% 
            left_join(dose_override_table %>% dplyr::select(source_concept_name_split, dose, unit), by = "source_concept_name_split")
            
        }else{
            data_prep <- data_prep %>% 
            mutate(dose = NA, unit = NA)
        }

        

        for(i in 1:nrow(translation_table_unit)){
            this_search <- translation_table_unit$str_search[i]
            this_translation <- translation_table_unit$translation[i]

            data_prep <- data_prep %>%
            mutate(unit = ifelse(is.na(unit) & grepl(this_search, source_concept_name_split, ignore.case = T, perl = T), 
                                 this_translation, 
                                 unit))
            rx_extract <- regex(
                paste0("\\b(\\d+(?:\\.\\d+)?)\\s*", this_translation, "\\b"),
                ignore_case = TRUE
            )

            data_prep <- data_prep %>%
            mutate(
                dose = if_else(
                    is.na(dose) & grepl(this_search, source_concept_name_split, ignore.case = T, perl = T),
                    as.numeric(str_match(source_concept_name_split, rx_extract)[, 2]),
                    dose))
        }

        data_prep <- data_prep %>%
        relocate(dose, .before = 9) %>%
        relocate(unit, .before = 10)

        unique_dosage_detail <- data_prep %>% 
        dplyr::select(source_concept_name, source_concept_name_split, concept_name_detail, dose, unit) %>% 
        distinct(source_concept_name, source_concept_name_split, concept_name_detail, dose, unit) %>% 
        arrange(concept_name_detail, str_count(source_concept_name, " / "), source_concept_name)
    }

    unassigned_concept_detail <- data_prep %>% 
    filter(is.na(concept_name_detail)) %>% 
    filter(!is.na(source_concept_name_split)) %>% 
    mutate(concept = paste0(source_concept_id, " | ", source_concept_name_split)) %>% 
    dplyr::select(concept) %>% table %>% sort(decreasing = T) %>% 
    as.data.frame

    unique_concept_detail <- data_prep %>% 
    dplyr::select(source_concept_name, source_concept_name_split, concept_name_detail) %>% 
    distinct(source_concept_name, source_concept_name_split, concept_name_detail) %>% 
    arrange(concept_name_detail, str_count(source_concept_name, " / "), source_concept_name)

    data_prep <- data_prep %>% filter(!is.na(concept_name_detail)) 

    if(!is.null(drug_specific_exclusions)){
        data_prep <- data_prep %>% filter(!(concept_name_detail %in% drug_specific_exclusions))
    }

    write.csv(unassigned_concept_detail, str_glue("Data_Prepped/Unassigned_Concept_Detail-All_Participants-{concept_name}-{table_type}-Translate.csv"), row.names = F)
    write.csv(unique_concept_detail, str_glue("Data_Prepped/Unique_Concept_Detail-All_Participants-{concept_name}-{table_type}-Translate.csv"), row.names = F)
    write.csv(unique_dosage_detail, str_glue("Data_Prepped/Unique_Dosage_Detail-All_Participants-{concept_name}-{table_type}-Translate.csv"), row.names = F)
    save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}-Translate.rds"))
    write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-{table_type}-Translate.txt"), row.names = F, col.names = F)

    if(verbose){data_prep %>% head}
}

# %%
atc_drugs <- read.csv("Drug_ATC_Categories.csv") %>% 
mutate(length = nchar(Name)) %>%
arrange(Name)

atc_drugs %>% head

# %%
translation_table_unit <- c("mg", "mg",
                            "ml", "ml",
                            "meq", "meq",
                            "mg/ml", "mg/ml",
                            "meq/ml", "meq/ml"
                           ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("str_search", "translation")) %>%
mutate(length = nchar(str_search)) %>%
arrange(-length, translation)

# %% [markdown]
# # Antidepressant ingredient names

# %%
# Load in the antidepressants table
concept_name <- "Antidepressants"
table_type <- "Drug_Exposure"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F) # data_prep

# %%
data_prep_drug_split <- data_prep %>%
  mutate(
    source_concept_name_split = as.character(source_concept_name)
  ) %>%
  separate_rows(
    source_concept_name_split,
    sep = " / "
  ) %>%
  mutate(
    source_concept_name_split = str_squish(source_concept_name_split)   # clean up extra whitespace
  )%>%
  relocate(source_concept_name_split, .before = 7)

# %%
# Identify all concepts in the table
all_concepts <- data_prep_drug_split %>% distinct(source_concept_name_split) %>% rename(source_concept_name = "source_concept_name_split")

# %%
# Define the string to search and sort by character length
translation_table_start <- data.frame(translation = atc_drugs %>% pull(Name) %>% unique) %>%
filter(!is.na(translation)) %>%
mutate(str_search = translation,
       length = nchar(str_search)
      ) %>%
arrange(-length, translation) %>%
dplyr::select(str_search, translation)

translation_table_start %>% head(15)

# %%
# Iterate by adding to translation_table_add until missing_translation is empty or contains only non-drug entries.
all_concepts_translate <- all_concepts %>% mutate(string_match = NA, concept_name_detail = NA)

translation_table_add <- c("paxil", "paroxetine",
                           "wellbutrin", "bupropion",
                           "zyban", "bupropion",
                           "cymbalta", "duloxetine",
                           "budeprion", "bupropion",
                           "luvox", "fluvoxamine",
                           "brisdelle", "paroxetine",
                           "desyrel", "trazodone",
                           "sarafem", "fluoxetine",
                           "buproban", "bupropion",
                           "anafranil", "clomipramine",
                           "aplenzin", "bupropion",
                           "oleptro", "trazodone",
                           "pexeva", "paroxetine",
                           "contrave", "bupropion / naltrexone",
                           "forfivo", "bupropion",
                           "limbitrol", "amitriptyline / chlordiazepoxide",
                           "brintellix", "vortioxetine",
                           "5-hydroxytryptophan", "amino acids",
                           "alanine", "amino acids",
                           "aminosyn", "amino acids",
                           "arginine", "amino acids",
                           "clinimix", "amino acids",
                           "clinisol", "amino acids",
                           "freamine", "amino acids",
                           "nephramine", "amino acids",
                           "prosol", "amino acids",
                           "travasol", "amino acids",
                           "velosulin", "insulin"
                           
                          ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("str_search", "translation"))

translation_table_comb <- rbind(translation_table_start, translation_table_add) %>%
mutate(length = nchar(str_search)) %>%
arrange(-length, translation)

for(i in 1:nrow(translation_table_comb)){
    this_search <- translation_table_comb$str_search[i]
    this_translation <- translation_table_comb$translation[i]

    all_concepts_translate <- all_concepts_translate %>%
    mutate(string_match = ifelse(is.na(string_match) & grepl(this_search, source_concept_name, ignore.case = T, perl = T), 
                                        this_search, 
                                        string_match),
          concept_name_detail = ifelse(is.na(concept_name_detail) & grepl(this_search, source_concept_name, ignore.case = T, perl = T), 
                                        this_translation, 
                                        concept_name_detail))
}

all_concepts_translate <- all_concepts_translate %>% arrange(concept_name_detail)

missing_translation <- all_concepts_translate %>%
filter(is.na(concept_name_detail))

missing_translation

# %%
# Grab only the translations that were used
translation_table_final <- all_concepts_translate %>% distinct(string_match, concept_name_detail) %>%
rename(str_search = "string_match", translation = "concept_name_detail") %>%
mutate(length = nchar(str_search)
      ) %>%
arrange(-length, translation) %>%
filter(!is.na(translation))

translation_table_final

# %%
# All translations should be manually double-checked.

write.csv(translation_table_final, "Data_Prepped/Translation_Table-Antidepressants.csv")

# %%
concept_name <- "Antidepressants"
table_type <- "Drug_Exposure"
dose_override_table <- read.csv(str_glue("dose_override-{concept_name}.csv"))
translate_table(concept_name, 
                table_type, 
                drug_category_file = "Drug_ATC_Categories.csv", 
                translation_table_unit = translation_table_unit,
                dose_override_table = dose_override_table,
                drug_category_filter_to = "ANTIDEPRESSANTS")

# Unique_Concept_Detail checked 01/27/2026

# %% [markdown]
# # Atypical Antipsychotic ingredient names

# %%
concept_name <- "Atypical_Antipsychotics"
table_type <- "Drug_Exposure"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F) # data_prep

# %%
data_prep_drug_split <- data_prep %>%
  mutate(
    source_concept_name_split = as.character(source_concept_name)
  ) %>%
  separate_rows(
    source_concept_name_split,
    sep = " / "
  ) %>%
  mutate(
    source_concept_name_split = str_squish(source_concept_name_split)   # clean up extra whitespace
  )%>%
  relocate(source_concept_name_split, .before = 7)

# %%
# Identify all concepts in the table
all_concepts <- data_prep_drug_split %>% distinct(source_concept_name_split) %>% rename(source_concept_name = "source_concept_name_split")

# %%
# Define the string to search and sort by character length
translation_table_start <- data.frame(translation = atc_drugs %>% pull(Name) %>% unique) %>%
filter(!is.na(translation)) %>%
mutate(str_search = translation,
       length = nchar(str_search)
      ) %>%
arrange(-length, translation) %>%
dplyr::select(str_search, translation)

translation_table_start %>% head(15)

# %%
# Iterate by adding to translation_table_add until missing_translation is empty or contains only non-drug entries.
all_concepts_translate <- all_concepts %>% mutate(string_match = NA, concept_name_detail = NA)

translation_table_add <- c("abilify", "aripiprazole",
                           "risperdal", "risperidone",
                           "perseris", "risperidone"
                           
                          ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("str_search", "translation"))

translation_table_comb <- rbind(translation_table_start, translation_table_add) %>%
mutate(length = nchar(str_search)) %>%
arrange(-length, translation)

for(i in 1:nrow(translation_table_comb)){
    this_search <- translation_table_comb$str_search[i]
    this_translation <- translation_table_comb$translation[i]

    all_concepts_translate <- all_concepts_translate %>%
    mutate(string_match = ifelse(is.na(string_match) & grepl(this_search, source_concept_name, ignore.case = T, perl = T), 
                                        this_search, 
                                        string_match),
          concept_name_detail = ifelse(is.na(concept_name_detail) & grepl(this_search, source_concept_name, ignore.case = T, perl = T), 
                                        this_translation, 
                                        concept_name_detail))
}

all_concepts_translate <- all_concepts_translate %>% arrange(concept_name_detail)

missing_translation <- all_concepts_translate %>%
filter(is.na(concept_name_detail))

missing_translation

# %%
# Grab only the translations that were used
translation_table_final <- all_concepts_translate %>% distinct(string_match, concept_name_detail) %>%
rename(str_search = "string_match", translation = "concept_name_detail") %>%
mutate(length = nchar(str_search)
      ) %>%
arrange(-length, translation) %>%
filter(!is.na(translation))

translation_table_final

# %%
# All translations should be manually double-checked.
write.csv(translation_table_final, "Data_Prepped/Translation_Table-Atypical_Antipsychotics.csv")

# %%
concept_name <- "Atypical_Antipsychotics"
table_type <- "Drug_Exposure"
dose_override_table <- read.csv(str_glue("dose_override-{concept_name}.csv"))
translate_table(concept_name, 
                table_type, 
                drug_category_file = "Drug_ATC_Categories.csv",
                translation_table_unit = translation_table_unit,
                dose_override_table = dose_override_table,
                drug_category_filter_to = "ANTIPSYCHOTICS")

# Unique_Concept_Detail checked 01/27/2026

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Atypical_Antipsychotics-Drug_Exposure-Translate.rds")

# %%
data_prep %>% filter(source_concept_name == "2 ML amisulpride 2.5 MG/ML Injection")

# %%
data_prep %>% filter(concept_name_detail == "aripiprazole") %>% count(source_concept_name_split) %>% arrange(-n)

# %%
data_prep %>% count(is.na(days_supply))

# %%
data_prep %>% count(is.na(quantity))

# %%
data_prep %>% count(is.na(quantity) | is.na(days_supply))
