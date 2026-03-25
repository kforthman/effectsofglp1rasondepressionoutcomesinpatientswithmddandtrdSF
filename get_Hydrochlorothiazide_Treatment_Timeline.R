# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

dte_cohort_data %>% head(10)

# %%
drug_class <- read.csv("DrugClasses.csv") %>%
mutate(Type = recode(Type, "Other" = "Other Antidepressants")) %>% 
rename(concept_category_level_5 = "Type")

drug_class %>%
head

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Hydrochlorothiazide-Drug_Exposure.rds", verbose = TRUE)
hc_drug_record <- data_prep

# %%
drug_matchedDS_info <- c("Insulins", "PS_Matched_Dataset-Insulins.rds",
                        "Metformin", "PS_Matched_Dataset-Metformin.rds",
                        "DPP4i", "PS_Matched_Dataset-DPP4i.rds",
                        "SGLT2i", "PS_Matched_Dataset-SGLT2i.rds",
                        "SU", "PS_Matched_Dataset-SU.rds",
                        #"TZD", "PS_Matched_Dataset-TZD.rds",
                        "GLP1RA", "PS_Matched_Dataset-GLP1RA.rds",
                        "Nontreatment", "PS_Matched_Dataset-Nontreatment.rds"
                        ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug", "file_path"))

drug_matchedDS_info

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i])
    
    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")

    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext)))) %>%
    dplyr::select(person_id, first_drug_record, treatment, treatment_name) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))


    var_name <- paste0("dte_cohort_ids_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, matched.data)
}

dte_cohort_ids_allSets <- rbind(
    dte_cohort_ids_Insulins,
    dte_cohort_ids_Metformin,
    dte_cohort_ids_DPP4i,
    dte_cohort_ids_SGLT2i,
    dte_cohort_ids_SU,
    #dte_cohort_ids_TZD,
    dte_cohort_ids_GLP1RA,
    dte_cohort_ids_Nontreatment
)

dte_cohort_ids_allSets <- dte_cohort_ids_allSets %>% mutate(study_cohort = as.factor(study_cohort))

dte_cohort_ids_allSets %>% head(10)

dte_cohort_ids_allSets %>% dim

dte_cohort_ids_allSets$study_cohort %>% table()

# %%
hc_drug_record %>% head

# %%
simp_drug_record <- hc_drug_record  %>%
arrange(person_id, concept_name, start_datetime) %>%
mutate(
    quantity = as.numeric(quantity),
    days_supply = as.numeric(days_supply)
)

# %%
simp_drug_record %>% head(200)

# %%
filename <- "hydrochlorothiazide_consecutive_instance.rds"
overwrite <- TRUE
if(!file.exists(filename) | overwrite){
    df <- simp_drug_record

    # Calculate intervals and instances
    df <- df %>%
      arrange(person_id, concept_name, start_datetime) %>%
      group_by(person_id, concept_name) %>%
      mutate(
        interval = as.numeric(difftime(start_datetime, lag(start_datetime), units = "days")),
        interval = ifelse(row_number() == 1, 1, interval),
        consecutive_instance = interval > 180,
        consecutive_instance = ifelse(row_number() == 1, TRUE, consecutive_instance),
        consecutive_instance = cumsum(consecutive_instance)
      )

    # Calculate first and last records
    consecutive_instance_tab <- df %>%
      group_by(person_id, concept_name, consecutive_instance) %>%
      summarise(
        first_record = first(start_datetime),
        last_record = last(start_datetime),
        last_record_end_datetime = last(end_datetime),
        last_record_refills = last(refills),
        last_record_quantity = last(quantity),
        last_record_days_supply = last(days_supply),
        total_collapsed_records = n(),
        .groups = 'drop'
      ) %>%
      dplyr::select(-consecutive_instance) %>%
      arrange(person_id, first_record, concept_name) %>%
      mutate(last_record = replace(last_record, last_record == first_record, as.Date(NA))) %>%
      mutate(
        effective_end = as.Date(ifelse(
          !is.na(last_record_end_datetime),
          last_record_end_datetime,
          ifelse(!is.na(last_record), last_record, first_record) +
            ifelse(!is.na(last_record_days_supply), last_record_days_supply, 30)
        ))
      ) %>%
      as.data.frame
      save(consecutive_instance_tab, file = filename)
}else{
    load(filename)
}

# %%
consecutive_instance_tab %>% head
