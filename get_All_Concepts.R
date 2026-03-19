# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %% [markdown]
# # Setup

# %%
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {file_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path, table_type) {
    if(table_type == "Condition_Occurrence"){
        col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), 
                          standard_vocabulary = col_character(), condition_type_concept_name = col_character(), 
                          stop_reason = col_character(), visit_occurrence_concept_name = col_character(), 
                          condition_source_value = col_character(), source_concept_name = col_character(), 
                          source_concept_code = col_character(), source_vocabulary = col_character(), 
                          condition_status_source_value = col_character(), condition_status_concept_name = col_character())
    }else if(table_type == "Drug_Exposure"){
        col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), 
                          standard_vocabulary = col_character(), drug_type_concept_name = col_character(), 
                          stop_reason = col_character(), sig = col_character(), route_concept_name = col_character(), 
                          lot_number = col_character(), visit_occurrence_concept_name = col_character(), 
                          drug_source_value = col_character(), source_concept_name = col_character(), 
                          source_concept_code = col_character(), source_vocabulary = col_character(), 
                          route_source_value = col_character(), dose_unit_source_value = col_character())
    }else if(table_type == "Measurement"){
        col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), 
                          standard_vocabulary = col_character(), measurement_type_concept_name = col_character(), 
                          operator_concept_name = col_character(), value_as_concept_name = col_character(), 
                          unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), 
                          measurement_source_value = col_character(), source_concept_name = col_character(), 
                          source_concept_code = col_character(), source_vocabulary = col_character(), 
                          unit_source_value = col_character(), value_source_value = col_character())
    }else if(table_type == "Procedure_Occurrence"){
        col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), 
                          standard_vocabulary = col_character(), procedure_type_concept_name = col_character(), 
                          modifier_concept_name = col_character(), visit_occurrence_concept_name = col_character(), 
                          procedure_source_value = col_character(), source_concept_name = col_character(), 
                          source_concept_code = col_character(), source_vocabulary = col_character(), 
                          modifier_source_value = col_character())
    }else if(table_type == "Observation"){
        col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), 
                          standard_vocabulary = col_character(), observation_type_concept_name = col_character(), 
                          value_as_string = col_character(), value_as_concept_name = col_character(), 
                          qualifier_concept_name = col_character(), unit_concept_name = col_character(), 
                          visit_occurrence_concept_name = col_character(), observation_source_value = col_character(), 
                          source_concept_name = col_character(), source_concept_code = col_character(), 
                          source_vocabulary = col_character(), unit_source_value = col_character(), 
                          qualifier_source_value = col_character(), value_source_value = col_character())
    }else if(table_type == "Visit"){
        col_types <- cols(person_id = col_character(), visit_occurrence_id = col_character(), visit_concept_id = col_character(), 
                          standard_concept_name = col_character(), standard_concept_code = col_character(), 
                          standard_vocabulary = col_character(), visit_start_datetime = col_character(), 
                          visit_end_datetime = col_character(), visit_type_concept_id = col_character(), 
                          visit_type_concept_name = col_character()
                         )
    }else if(table_type == "Person"){
        col_types <- cols(person_id = col_character(), gender_concept_id = col_character(), gender = col_character(),
                          date_of_birth = col_character(), race_concept_id = col_character(), 
                          race = col_character(), ethnicity_concept_id = col_character(), ethnicity = col_character(),
                          sex_at_birth_concept_id = col_character(), sex_at_birth = col_character(), 
                          self_reported_category_concept_id = col_character(), self_reported_category = col_character()
                         )
    }
        bind_rows(
            map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
                function(csv) {
                    message(str_glue('Loading {csv}.'))
                    chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
                    if (is.null(col_types)) {
                        col_types <- spec(chunk)
                    }
                    chunk
                }))
        }

        get_sql_concept <- function(concept_name, table_type, verbose = F, overwrite = F){
            sql_query <- read_file(str_glue("SQL_Query-All_Participants-{concept_name}-{table_type}.txt"))
            if(verbose){
                message(str_glue("SQL_Query-All_Participants-{concept_name}-{table_type}.txt\n"))
                message(str_glue("SQL Query:\n", sql_query, "\n"))
            }

            # Formulate a Cloud Storage destination path for the data exported from BigQuery.
            # NOTE: By default data exported multiple times on the same day will overwrite older copies.
            #       But data exported on a different days will write to a new location so that historical
            #       copies can be kept as the dataset definition is changed.
            file_path <- file.path(
                Sys.getenv("WORKSPACE_BUCKET"),
                "bq_exports",
                Sys.getenv("OWNER_EMAIL"),
                str_glue("All_Participants-{concept_name}-{table_type}-*.csv"))
            message(str_glue('The data will be written to {file_path}. Use this path when reading ',
                             'the data into your notebooks in the future.'))

            if(overwrite){
                gs_delete(file.path(
                    "bq_exports",
                    Sys.getenv("OWNER_EMAIL"),
                    str_glue("All_Participants-{concept_name}-{table_type}-*.csv")), 
                          Sys.getenv("WORKSPACE_BUCKET"))
            }

            # Perform the query and export the dataset to Cloud Storage as CSV files.
            # NOTE: You only need to run `bq_table_save` once. After that, you can
            #       just read data from the CSVs in Cloud Storage.
            bq_table_save(
                bq_dataset_query(
                    Sys.getenv("WORKSPACE_CDR"), 
                    sql_query, 
                    billing = Sys.getenv("GOOGLE_PROJECT")
                ),
                file_path,
                destination_format = "CSV")



            dataset_preview <- read_bq_export_from_workspace_bucket(file_path, table_type)

            dim(dataset_preview)

            head(dataset_preview, 15)
        }

        import_data <- function(concept_name, table_type){
            file_path <- file.path(
                Sys.getenv("WORKSPACE_BUCKET"),
                "bq_exports",
                Sys.getenv("OWNER_EMAIL"),
                str_glue("All_Participants-{concept_name}-{table_type}-*.csv"))
            message(str_glue('The data will be written to {file_path}. Use this path when reading ',
                             'the data into your notebooks in the future.'))

            read_bq_export_from_workspace_bucket(file_path, table_type)

        }

        prep_save_condition_table <- function(concept_name, concept_name_simp = NA, verbose = TRUE){
            concept_name_simp <- ifelse(is.na(concept_name_simp), concept_name, concept_name_simp)
            data <- import_data(concept_name, "Condition_Occurrence")

            data_prep <- data %>% dplyr::select(person_id, visit_occurrence_id,
                                                condition_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                                                condition_type_concept_id, condition_type_concept_name,
                                                visit_occurrence_concept_name, condition_start_datetime, condition_end_datetime, stop_reason
                                               ) %>% 
            mutate(across(c(person_id, visit_occurrence_id), ~as.character(.))) %>%
            mutate(across(c(condition_source_concept_id, source_concept_code, source_vocabulary, source_concept_name, condition_type_concept_id, 
                            condition_type_concept_name, visit_occurrence_concept_name), ~as.factor(.))) %>%
            mutate(across(c(condition_start_datetime, condition_end_datetime), ~as.Date(.))) %>%
            mutate(concept_name = concept_name_simp) %>%
            mutate(origin_table_type = "Condition Occurrence") %>%
            rename(source_concept_id = condition_source_concept_id, 
                   type_concept_id = condition_type_concept_id, 
                   type_concept_name = condition_type_concept_name,
                   start_datetime = condition_start_datetime, 
                   end_datetime = condition_end_datetime
                  ) %>%
            arrange(person_id, visit_occurrence_id, start_datetime)

            save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Condition_Occurrence.rds"))
            write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Condition_Occurrence.txt"), row.names = F, col.names = F)

            if(verbose){data_prep %>% head}
        }

    prep_save_observation_table <- function(concept_name, concept_name_simp = NA, verbose = TRUE){
        concept_name_simp <- ifelse(is.na(concept_name_simp), concept_name, concept_name_simp)
        data <- import_data(concept_name, "Observation")

        data_prep <- data %>% dplyr::select(person_id, visit_occurrence_id,
                                            observation_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                                            observation_type_concept_id, observation_type_concept_name,
                                            qualifier_concept_id, qualifier_concept_name,
                                            visit_occurrence_concept_name, observation_datetime
                                           ) %>% 
        mutate(across(c(person_id, visit_occurrence_id), ~as.character(.))) %>%
        mutate(across(c(observation_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                        observation_type_concept_id, observation_type_concept_name,
                        qualifier_concept_id, qualifier_concept_name,
                        visit_occurrence_concept_name), ~as.factor(.))) %>%
        mutate(across(c(observation_datetime), ~as.Date(.))) %>%
        mutate(concept_name = concept_name_simp) %>%
        mutate(origin_table_type = "Observation") %>%
        rename(source_concept_id = observation_source_concept_id, 
               type_concept_id = observation_type_concept_id, 
               type_concept_name = observation_type_concept_name,
               start_datetime = observation_datetime
              ) %>%
        arrange(person_id, visit_occurrence_id, start_datetime)

        save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Observation.rds"))
        write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Observation.txt"), row.names = F, col.names = F)

        if(verbose){data_prep %>% head}
    }

    prep_save_measurement_table <- function(concept_name, concept_name_simp = NA, verbose = TRUE){
        concept_name_simp <- ifelse(is.na(concept_name_simp), concept_name, concept_name_simp)
        data <- import_data(concept_name, "Measurement")

        data_prep <- data %>% dplyr::select(person_id, visit_occurrence_id,
                                            measurement_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                                            measurement_type_concept_id, measurement_type_concept_name,
                                            value_as_number, value_as_concept_id, value_as_concept_name, unit_concept_id, unit_concept_name, range_low, range_high,
                                            visit_occurrence_concept_name, measurement_datetime
                                           ) %>% 
        mutate(across(c(person_id, visit_occurrence_id, value_as_number, range_low, range_high), ~as.character(.))) %>%
        mutate(across(c(measurement_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                        measurement_type_concept_id, measurement_type_concept_name,
                        value_as_concept_id, value_as_concept_name, unit_concept_id, unit_concept_name, visit_occurrence_concept_name
                       ), ~as.factor(.))) %>%
        mutate(across(c(measurement_datetime), ~as.Date(.))) %>%
        mutate(concept_name = concept_name_simp) %>%
        mutate(origin_table_type = "Measurement") %>%
        rename(source_concept_id = measurement_source_concept_id, 
               type_concept_id = measurement_type_concept_id, 
               type_concept_name = measurement_type_concept_name,
               start_datetime = measurement_datetime
              ) %>%
        arrange(person_id, visit_occurrence_id, start_datetime)

        save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Measurement.rds"))
        write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Measurement.txt"), row.names = F, col.names = F)

        if(verbose){data_prep %>% head}
    }

    prep_save_drug_table <- function(concept_name, concept_name_simp = NA, verbose = TRUE){
        concept_name_simp <- ifelse(is.na(concept_name_simp), concept_name, concept_name_simp)
        data <- import_data(concept_name, "Drug_Exposure")
        data_prep <- data %>% dplyr::select(person_id, visit_occurrence_id,
                                            drug_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                                            drug_type_concept_id, drug_type_concept_name,
                                            stop_reason, refills, quantity, days_supply, route_concept_id, route_concept_name,
                                            visit_occurrence_concept_name, drug_exposure_start_datetime, drug_exposure_end_datetime
                                           ) %>% 
        mutate(across(c(person_id, visit_occurrence_id, refills, quantity, days_supply), ~as.character(.))) %>%
        mutate(across(c(
            drug_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
            drug_type_concept_id, drug_type_concept_name,
            stop_reason, route_concept_id, route_concept_name,
            visit_occurrence_concept_name
        ), ~as.factor(.))) %>%
        mutate(across(c(drug_exposure_start_datetime, drug_exposure_end_datetime), ~as.Date(.))) %>%
        mutate(concept_name = concept_name_simp) %>%
        mutate(origin_table_type = "Drug Exposure") %>%
        rename(source_concept_id = drug_source_concept_id, 
               type_concept_id = drug_type_concept_id, 
               type_concept_name = drug_type_concept_name,
               start_datetime = drug_exposure_start_datetime, 
               end_datetime = drug_exposure_end_datetime
              ) %>%
        arrange(person_id, visit_occurrence_id, start_datetime)

        save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Drug_Exposure.rds"))
        write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Drug_Exposure.txt"), row.names = F, col.names = F)

        if(verbose){data_prep %>% head}
    }

    prep_save_procedure_table <- function(concept_name, concept_name_simp = NA, verbose = TRUE){
        concept_name_simp <- ifelse(is.na(concept_name_simp), concept_name, concept_name_simp)
        data <- import_data(concept_name, "Procedure_Occurrence")
        data_prep <- data %>% dplyr::select(person_id, visit_occurrence_id,
                                            procedure_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                                            procedure_type_concept_id, procedure_type_concept_name,
                                            visit_occurrence_concept_name, procedure_datetime
                                           ) %>% 
        mutate(across(c(person_id, visit_occurrence_id), ~as.character(.))) %>%
        mutate(across(c(procedure_source_concept_id, source_concept_code, source_vocabulary, source_concept_name,
                        procedure_type_concept_id, procedure_type_concept_name,
                        visit_occurrence_concept_name), ~as.factor(.))) %>%
        mutate(across(c(procedure_datetime), ~as.Date(.))) %>%
        mutate(concept_name = concept_name_simp) %>%
        mutate(origin_table_type = "Procedure Occurrence") %>%
        rename(source_concept_id = procedure_source_concept_id, 
               type_concept_id = procedure_type_concept_id, 
               type_concept_name = procedure_type_concept_name,
               start_datetime = procedure_datetime
              ) %>%
        arrange(person_id, visit_occurrence_id, start_datetime)

        save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Procedure_Occurrence.rds"))
        write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Procedure_Occurrence.txt"), row.names = F, col.names = F)

        if(verbose){data_prep %>% head}
    }

    prep_save_visit_table <- function(concept_name, concept_name_simp = NA, verbose = TRUE){
        concept_name_simp <- ifelse(is.na(concept_name_simp), concept_name, concept_name_simp)
        data <- import_data(concept_name, "Visit")
        data_prep <- data %>% dplyr::select(person_id, visit_occurrence_id, visit_concept_id, standard_concept_name, standard_concept_code, 
                                            standard_vocabulary, visit_start_datetime, visit_end_datetime, 
                                            visit_type_concept_id, visit_type_concept_name
                                           ) %>% 
        mutate(across(c(person_id, visit_occurrence_id, visit_concept_id, standard_concept_name, standard_concept_code, 
                        standard_vocabulary, visit_type_concept_id, 
                        visit_type_concept_name), ~as.character(.))) %>%
        mutate(across(c(visit_start_datetime, visit_end_datetime), ~as.Date(.))) %>%
        mutate(concept_name = concept_name_simp) %>%
        mutate(origin_table_type = "Visit") %>%
        rename(concept_id = visit_concept_id,
               type_concept_id = visit_type_concept_id, 
               type_concept_name = visit_type_concept_name,
               start_datetime = visit_start_datetime,
               end_datetime = visit_end_datetime
              ) %>%
        arrange(person_id, visit_occurrence_id, start_datetime)

        save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Visit.rds"))
        write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Visit.txt"), row.names = F, col.names = F)

        if(verbose){data_prep %>% head}
    }

    prep_save_person_table <- function(concept_name, concept_name_simp = NA, verbose = TRUE){
        concept_name_simp <- ifelse(is.na(concept_name_simp), concept_name, concept_name_simp)
        data <- import_data(concept_name, "Person")
        data_prep <- data %>% dplyr::select(person_id, gender_concept_id, gender, date_of_birth, 
                                            race_concept_id, race, ethnicity_concept_id, ethnicity,
                                            sex_at_birth_concept_id, sex_at_birth, 
                                            self_reported_category_concept_id, self_reported_category 
                                           ) %>% 
        mutate(across(c(person_id, gender_concept_id, gender,race_concept_id, race, ethnicity_concept_id, 
                        ethnicity, sex_at_birth_concept_id, sex_at_birth, self_reported_category_concept_id, 
                        self_reported_category), ~as.character(.))) %>%
        mutate(across(c(date_of_birth), ~as.Date(.))) %>%
        mutate(concept_name = concept_name_simp) %>%
        mutate(origin_table_type = "Person") %>%
        arrange(person_id)

        save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Person.rds"))
        write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Person.txt"), row.names = F, col.names = F)

        if(verbose){data_prep %>% head}
    }

    translate_table <- function(concept_name, table_type, verbose = TRUE, drug_category_file = NULL, 
                                drug_category_filter_to = NULL, drug_specific_exclusions = NULL){

        load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)

        trans_tab_file <- str_glue("Data_Prepped/Translation_Table-{concept_name}.csv")

        if(!file.exists(trans_tab_file)){
            stop(str_glue("Translation table file does not exist. {trans_tab_file}"))
        }

        trans_tab <- read.csv(str_glue("Data_Prepped/Translation_Table-{concept_name}.csv"))

        data_prep <- data_prep %>% mutate(concept_name_detail = NA)

        for(i in 1:nrow(trans_tab)){
            data_prep <- data_prep %>%
            mutate(concept_name_detail = ifelse(grepl(trans_tab$str_search[i], source_concept_name, ignore.case = T, perl = TRUE) & is.na(concept_name_detail), 
                                                trans_tab$translation[i], 
                                                concept_name_detail))
        }

        if(!is.null(drug_category_file)){
            drug_class <- read.csv("Drug_ATC_Categories.csv") %>% arrange(Name)

            drug_class <- rbind(drug_class,
                                (data.frame(Name = unique(data_prep$concept_name_detail)) %>%
                                 filter(grepl("/",Name)) %>%
                                 rowwise() %>%
                                 mutate(Category_Level_4 = paste(unique(drug_class$Category_Level_4[drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
                                 mutate(Category_Level_3 = paste(unique(drug_class$Category_Level_3[drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
                                 mutate(Category_Level_2 = paste(unique(drug_class$Category_Level_2[drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
                                 mutate(Category_Level_1 = paste(unique(drug_class$Category_Level_1[drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / ")) %>%
                                 mutate(ATC_code =              paste(unique(   drug_class$ATC_code[drug_class$Name %in% str_split_1(Name, " / ")]), collapse = " / "))
                                )
                               )

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

        unassigned_concept_detail <- data_prep %>% 
        filter(is.na(concept_name_detail)) %>% 
        filter(!is.na(source_concept_name)) %>% 
        mutate(concept = paste0(source_concept_id, " | ", source_concept_name)) %>% 
        dplyr::select(concept) %>% table %>% sort(decreasing = T) %>% 
        as.data.frame

        unique_concept_detail <- data_prep %>% 
        dplyr::select(source_concept_name, concept_name_detail) %>% 
        group_by(source_concept_name, concept_name_detail) %>% ## Suggested change:
        summarize() %>%                                        ## replace with `distinct()`
        arrange(str_count(concept_name_detail, " / "), concept_name_detail, source_concept_name)

        data_prep <- data_prep %>% filter(!is.na(concept_name_detail)) 

        if(!is.null(drug_specific_exclusions)){
            data_prep <- data_prep %>% filter(!(concept_name_detail %in% drug_specific_exclusions))
        }

        write.csv(unassigned_concept_detail, str_glue("Data_Prepped/Unassigned_Concept_Detail-All_Participants-{concept_name}-{table_type}.csv"), row.names = F)
        write.csv(unique_concept_detail, str_glue("Data_Prepped/Unique_Concept_Detail-All_Participants-{concept_name}-{table_type}.csv"), row.names = F)
        save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"))
        write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-{table_type}.txt"), row.names = F, col.names = F)

        if(verbose){data_prep %>% head}
    }

# %%
#concept_name <- "Psychostimulants_Agents_used_for_ADHD_and_Nootropics"
#table_type <- "Drug_Exposure"
#load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
#data_prep %>% filter(grepl(" / ", source_concept_name, ignore.case = T, perl = TRUE)) %>% pull(source_concept_name) %>% unique %>% sort %>% paste(collapse = "\n") %>% cat

# %% [markdown]
# # Eligibility Criteria

# %% [markdown]
# ## Diagnosis

# %% [markdown]
# ### **Type 2 Diabetes Mellitus**

# %%
concept_name <- "Type_2_Diabetes_Mellitus"
concept_name_simp <- "Type 2 Diabetes Mellitus"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, concept_name_simp)

# %% [markdown]
# ### **Depression**

# %%
concept_name <- "Depression"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ## Had at least one of the diseases based on the prescription guideline for semaglutide

# %% [markdown]
# ### **Obesity**

# %%
concept_name <- "Obesity"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %%
concept_name <- "Obesity"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Condition_Occurrence.rds"), verbose = F)
data_prep_co <- data_prep
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Observation.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ### **Hypertension**

# %%
concept_name <- "Hypertension"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Hypercholesterolemia**

# %%
concept_name <- "Hypercholesterolemia"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Hyperlipidemia**

# %%
concept_name <- "Hyperlipidemia"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Heart Disease**

# %%
concept_name <- "Heart_Disease"
concept_name_simp <- "Heart Disease"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, concept_name_simp)

# %% [markdown]
# ### **Stroke**

# %%
concept_name <- "Stroke"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %%
concept_name <- "Stroke"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Condition_Occurrence.rds"), verbose = F)
data_prep_co <- data_prep
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Observation.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ### **Chronic Kidney Disease**

# %%
concept_name <- "Chronic_Kidney_Disease"
concept_name_simp <- "Chronic Kidney Disease"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, concept_name_simp)

# %% [markdown]
# ### **A1C >= 8.5%**

# %%
concept_name <- "A1C"
table_type <- "Measurement"
get_sql_concept(concept_name, table_type)
prep_save_measurement_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Measurement.rds"), verbose = T)

# %%
data_prep %>% pull(unit_concept_name) %>% table %>% sort(decreasing = T)
data_prep %>% pull(unit_concept_id) %>% table %>% sort(decreasing = T)

# %%
data_prep <- data_prep %>% 
mutate(unit_concept_name_simp = ifelse(unit_concept_name %in% c("percent", "Percent", "Of Total H", "percent hemoglobin", "Percentage unit", "Percentage of total", "Percentage total hemoglobin", "percent hemoglobin A1c"), "percent", "unknown")) %>%
filter(unit_concept_name_simp == "percent" & !is.na(value_as_number)) %>%
mutate(A1C_over_8.5 = ifelse((unit_concept_name_simp == "percent" & !is.na(value_as_number) & as.numeric(value_as_number) >= 8.5 & as.numeric(value_as_number) <= 100), TRUE,
                             ifelse((unit_concept_name_simp == "percent" & !is.na(value_as_number) & as.numeric(value_as_number) < 8.5  & as.numeric(value_as_number) >= 0), FALSE,
                             NA
                                   )
                             )
      )

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Measurement.rds"))
write.table(unique(data_prep %>% filter(A1C_over_8.5) %>% pull(person_id)), str_glue("Data_Prepped/IDs-{concept_name}_over_8p5-Measurement.txt"), row.names = F, col.names = F)

# %% [markdown]
# ## No contraindication, warning, and limited use where one drug would be preferred over the other

# %% [markdown]
# ### **Pancreatitis**

# %%
concept_name <- "Pancreatitis"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Type 1 Diabetes**

# %%
concept_name <- "Type_1_Diabetes"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Thyroid Cancer**

# %%
concept_name <- "Thyroid_Cancer"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %%
concept_name <- "Thyroid_Cancer"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Condition_Occurrence.rds"), verbose = F)
data_prep_co <- data_prep
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Observation.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ### **Gastroparesis**

# %%
concept_name <- "Gastroparesis"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ## Exposure definitions

# %% [markdown]
# ### **Semaglutide**

# %%
concept_name <- "Semaglutide"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name)

# %% [markdown]
# ### **Insulins** 

# %%
concept_name <- "Insulins"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name)

# %% [markdown]
# ### **Metformin**

# %%
concept_name <- "Metformin"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name)

# %% [markdown]
# ### **DPP-4i**

# %%
concept_name <- "DPP_4i"
concept_name_simp <- "DPP-4i"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, concept_name_simp)

# %% [markdown]
# ### **SGLT2i**

# %%
concept_name <- "SGLT2i"
concept_name_simp <- "SGLT-2i"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, concept_name_simp)

# %% [markdown]
# ### **SU**

# %%
concept_name <- "SU"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name)

# %% [markdown]
# ### **TZD**

# %%
concept_name <- "TZD"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name)

# %% [markdown]
# ### **GLP-1RA**

# %%
concept_name <- "GLP_1RAs_Excluding_Semaglutide"
concept_name_simp <- "GLP-1RAs excluding Semaglutide"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, concept_name_simp)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-GLP_1RAs_Excluding_Semaglutide-Drug_Exposure.rds"), verbose = F)

# %%
data_prep <- data_prep %>%
mutate(concept_name_detail = ifelse(grepl("Albiglutide", source_concept_name, ignore.case = T), "Albiglutide", NA),
       concept_name_detail = ifelse(grepl("Dulaglutide", source_concept_name, ignore.case = T), "Dulaglutide", concept_name_detail),
       concept_name_detail = ifelse(grepl("Exenatide", source_concept_name, ignore.case = T), "Exenatide", concept_name_detail),
       concept_name_detail = ifelse(grepl("Liraglutide", source_concept_name, ignore.case = T), "Liraglutide", concept_name_detail),
       concept_name_detail = ifelse(grepl("Lixisenatide", source_concept_name, ignore.case = T), "Lixisenatide", concept_name_detail)
      ) %>%
filter(!is.na(concept_name_detail))

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Drug_Exposure.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Drug_Exposure.txt"), row.names = F, col.names = F)

# %% [markdown]
# # Outcomes

# %% [markdown]
# ## Primary Outcomes

# %% [markdown]
# ### **Antidepressants**

# %%
concept_name <- "Antidepressants"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, verbose = F)

# %%
concept_name <- "Atypical_Antipsychotics"
concept_name_simp <- "Atypical Antipsychotics"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, concept_name_simp)

# %% [markdown]
# ## Outcomes for Sensitivity Analysis

# %% [markdown]
# ### **Overall Medical visits**

# %%
concept_name <- "All_Visits"
table_type <- "Visit"
get_sql_concept(concept_name, table_type, verbose = TRUE, overwrite = T)
prep_save_visit_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-All_Visits-Visit.rds"), verbose = F)

data_prep <- data_prep %>% group_by(person_id) %>% summarize(n = n())

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Total-All_Participants-All_Visits-Visit.rds"))

# %% [markdown]
# ### **Outpatient medical visits**

# %%
load("Data_Prepped/Prepped_Data-All_Participants-All_Visits-Visit.rds", verbose = T)

# %%
data_prep <- data_prep %>% filter(standard_concept_name == "Outpatient Visit")

# %%
save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-Outpatient_Visits-Visit.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-Outpatient_Visits-Visit.txt"), row.names = F, col.names = F)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-Outpatient_Visits-Visit.rds"), verbose = F)

data_prep <- data_prep %>% group_by(person_id) %>% summarize(n = n())

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Total-All_Participants-Outpatient_Visits-Visit.rds"))

# %% [markdown]
# ### **Psychological or Psychiatric visits**

# %%
concept_name <- "Psychologic_or_Psychiatric_Visits_v2"
table_type <- "Observation"
get_sql_concept(concept_name, table_type, overwrite = T)
prep_save_observation_table(concept_name)

# %%
concept_name <- "Psychologic_or_Psychiatric_Visits_v2"
table_type <- "Procedure_Occurrence"
get_sql_concept(concept_name, table_type, overwrite = T)
prep_save_procedure_table(concept_name)

# %%
concept_name <- "Psychologic_or_Psychiatric_Visits_v2"
table_type <- "Visit"
get_sql_concept(concept_name, table_type, overwrite = T)
prep_save_visit_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Observation.rds"), verbose = F)
data_prep_o <- data_prep
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Procedure_Occurrence.rds"), verbose = F)
data_prep_po <- data_prep

data_prep <- bind_rows(data_prep_o, data_prep_po) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %%
concept_name <- "Psychologic_or_Psychiatric_Procedure_or_Service"
table_type <- "Procedure_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_procedure_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-Psychologic_or_Psychiatric_Procedure_or_Service-Procedure_Occurrence.rds"), verbose = F)

data_prep <- data_prep %>% group_by(person_id) %>% summarize(n = n())

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Total-All_Participants-Psychologic_or_Psychiatric_Procedure_or_Service-Procedure_Occurrence.rds"))

# %% [markdown]
# ## Other Outcomes

# %% [markdown]
# ### Hydrochlorothiazide

# %%
concept_name <- "Hydrochlorothiazide"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, verbose = F)

# %% [markdown]
# # Propensity Matching Covariates

# %% [markdown]
# ## Demographics

# %% [markdown]
# ### **Age, Sex, and Race/Ethnicity**

# %%
concept_name <- "Demographics"
table_type <- "Person"
get_sql_concept(concept_name, table_type)
prep_save_person_table(concept_name)

# %%
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Person.rds"), verbose = T)

# %%
data_prep <- data_prep %>%
mutate(gender_simp = ifelse(gender == 'Male', 'Male', NA),
       gender_simp = ifelse(gender == 'Female', 'Female', gender_simp),
       gender_simp = ifelse(gender == 'PMI: Skip', NA, gender_simp),
       gender_simp = ifelse(gender == 'Not man only, not woman only, prefer not to answer, or skipped', NA, gender_simp),
       gender_simp = ifelse(gender == 'Gender Identity: Transgender', 'Transgender', gender_simp),
       gender_simp = ifelse(gender == 'Gender Identity: Additional Options', NA, gender_simp),
       gender_simp = ifelse(gender == 'Gender Identity: Non Binary', 'Non Binary', gender_simp),
       gender_simp = ifelse(gender == 'I prefer not to answer', NA, gender_simp),
       gender_simp = ifelse(gender == 'No matching concept', NA, gender_simp)
      ) %>%
mutate(
       race_simp = ifelse(race == 'Black or African American', 'Black or African American', NA),
       race_simp = ifelse(race == 'White', 'White', race_simp),
       race_simp = ifelse(race == 'None Indicated', NA, race_simp),
       race_simp = ifelse(race == 'Asian', 'Asian', race_simp),
       race_simp = ifelse(race == 'American Indian or Alaska Native', 'American Indian or Alaska Native', race_simp),
       race_simp = ifelse(race == 'Middle Eastern or North African', 'Middle Eastern or North African', race_simp),
       race_simp = ifelse(race == 'More than one population', 'Multi Race', race_simp),
       race_simp = ifelse(race == 'PMI: Skip', NA, race_simp),
       race_simp = ifelse(race == 'None of these', NA, race_simp),
       race_simp = ifelse(race == 'I prefer not to answer', NA, race_simp),
       race_simp = ifelse(race == 'Native Hawaiian or Other Pacific Islander', 'Native Hawaiian or Other Pacific Islander', race_simp)
) %>%
mutate(
       ethnicity_simp = ifelse(ethnicity == 'Not Hispanic or Latino', 'Not Hispanic or Latino', NA),
       ethnicity_simp = ifelse(ethnicity == 'Hispanic or Latino', 'Hispanic or Latino', ethnicity_simp),
       ethnicity_simp = ifelse(ethnicity == 'PMI: Skip', NA, ethnicity_simp),
       ethnicity_simp = ifelse(ethnicity == 'What Race Ethnicity: Race Ethnicity None Of These', NA, ethnicity_simp),
       ethnicity_simp = ifelse(ethnicity == 'PMI: Prefer Not To Answer', NA, ethnicity_simp),
       ethnicity_simp = ifelse(ethnicity == 'No matching concept', NA, ethnicity_simp),
) %>% 
mutate(
       sex_simp = ifelse(sex_at_birth == 'Male', 'Male', NA),
       sex_simp = ifelse(sex_at_birth == 'Female', 'Female', sex_simp),
       sex_simp = ifelse(sex_at_birth == 'PMI: Skip', NA, sex_simp),
       sex_simp = ifelse(sex_at_birth == 'I prefer not to answer', NA, sex_simp),
       sex_simp = ifelse(sex_at_birth == 'Intersex', 'Intersex', sex_simp),
       sex_simp = ifelse(sex_at_birth == 'No matching concept', NA, sex_simp),
       sex_simp = ifelse(sex_at_birth == 'Sex At Birth: Sex At Birth None Of These', NA, sex_simp)
) %>%
mutate(race_ethnicity = ifelse(ethnicity_simp == "Hispanic or Latino", "Hispanic or Latino", race_simp)) %>%
mutate(age = round(interval(date_of_birth, as.Date("2023-10-23")) / dyears())) %>% # October 1, 2023 is the cutoff date for v8
mutate(age_group_4 = cut(age, 
                         breaks = c(-Inf, 34, 44, 64, Inf), 
                         labels = c("18 - 34 years", "35 - 44 years", "45 - 64 years", "65 years and over"), 
                         right = TRUE, include.lowest = TRUE),
      age_group_5 = cut(age, 
                        breaks = c(-Inf, 24, 34, 44, 64, Inf), 
                        labels = c("18 - 24 years", "25 - 34 years", "35 - 44 years", "45 - 64 years", "65 years and over"), 
                        right = TRUE, include.lowest = TRUE)
      ) %>%
mutate(across(c(gender_simp, race_simp, ethnicity_simp, sex_simp, age_group_4, age_group_5), ~as.factor(.)))
       
       data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Person.rds"))

# %% [markdown]
# ### **Persons with Potential Health Hazards Related to Socioeconomic and Psychosocial Circumstances**

# %%
concept_name <- "Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Persons with Potential Health Hazards Related to Socioeconomic and Psychosocial Circumstances")

# %%
concept_name <- "Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name, "Persons with Potential Health Hazards Related to Socioeconomic and Psychosocial Circumstances")

# %%
concept_name <- "Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Condition_Occurrence.rds"), verbose = F)
data_prep_co <- data_prep
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Observation.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ### **Problems Related to Lifestyle**

# %%
concept_name <- "Problems_Related_to_Lifestyle"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Problems Related to Lifestyle")

# %%
concept_name <- "Problems_Related_to_Lifestyle"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name, "Problems Related to Lifestyle")

# %%
concept_name <- "Problems_Related_to_Lifestyle"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Condition_Occurrence.rds"), verbose = F)
data_prep_co <- data_prep
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Observation.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ## Problems Related to Type 2 Diabetes

# %% [markdown]
# ### **Type 2 Diabetes Mellitus with Complications**
#
#  - with Hyperclycemia
#  - with Neurological Complications
#  - with Circulatory Complications
#  - with Kidney Complications
#  - with Ophthalmic Complications
#  - with Other Specified Complications
#  - with Other Unspecified Complications

# %%
concept_name <- "Type_2_Diabetes_Mellitus_with_Complications"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Type 2 Diabetes Mellitus with Complications")

# %% [markdown]
# ### **Obesity**
#
# *See above*

# %% [markdown]
# ### **Bariatric Surgery Status**

# %%
concept_name <- "Bariatric_Surgery_Status"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name, "Bariatric Surgery Status")

# %% [markdown]
# ### **Bariatric Surgery**

# %%
concept_name <- "Bariatric_Surgery"
table_type <- "Procedure_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_procedure_table(concept_name, "Bariatric Surgery")

# %%
concept_name <- "Bariatric_Surgery"
table_type <- "Procedure_Occurrence"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_co <- data_prep
concept_name <- "Bariatric_Surgery_Status"
table_type <- "Observation"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ## Psychiatric disorders

# %% [markdown]
# ### **Attention-Deficit/Hyperactivity Disorder (ADHD)**

# %%
concept_name <- "ADHD"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Agoraphobia**

# %%
concept_name <- "Agoraphobia"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Anxiety Disorder NOS**

# %%
concept_name <- "Anxiety_Disorder_NOS"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Anxiety Disorder NOS")

# %% [markdown]
# ### **Generalized Anxiety**

# %%
concept_name <- "Generalized_Anxiety"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Generalized Anxiety")

# %% [markdown]
# ### **OCD**

# %%
concept_name <- "OCD"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name)

# %% [markdown]
# ### **Panic Disorder**

# %%
concept_name <- "Panic_Disorder"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Panic Disorder")

# %% [markdown]
# ### **PTSD**

# %%
concept_name <- "PTSD"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "PTSD")

# %% [markdown]
# ### **Social Anxiety Disorder**

# %%
concept_name <- "Social_Anxiety_Disorder"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Social Anxiety Disorder")

# %% [markdown]
# ## Substance abuse and dependence

# %% [markdown]
# ### **Alcohol Abuse**

# %%
concept_name <- "Alcohol_Abuse"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Alcohol Abuse")

# %%
concept_name <- "Alcohol_Abuse"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name, "Alcohol Abuse")

# %%
concept_name <- "Alcohol_Abuse"
table_type <- "Condition_Occurrence"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_co <- data_prep
table_type <- "Observation"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ### **Alcohol Dependence**

# %%
concept_name <- "Alcohol_Dependence"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Alcohol Dependence")

# %%
concept_name <- "Alcohol_Dependence"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name, "Alcohol Dependence")

# %%
concept_name <- "Alcohol_Dependence"
table_type <- "Condition_Occurrence"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_co <- data_prep
table_type <- "Observation"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ### **Cannabis Abuse**

# %%
concept_name <- "Cannabis_Abuse"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Cannabis Abuse")

# %% [markdown]
# ### **Cannabis Dependence**

# %%
concept_name <- "Cannabis_Dependence"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Cannabis Dependence")

# %% [markdown]
# ### **Cocaine Abuse**

# %%
concept_name <- "Cocaine_Abuse"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Cocaine Abuse")

# %% [markdown]
# ### **Cocaine Dependence**

# %%
concept_name <- "Cocaine_Dependence"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Cocaine Dependence")

# %% [markdown]
# ### **Opioid Abuse**

# %%
concept_name <- "Opioid_Abuse"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Opioid Abuse")

# %% [markdown]
# ### **Opioid Dependence**

# %%
concept_name <- "Opioid_Dependence"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Opioid Dependence")

# %% [markdown]
# ### **Sedative Abuse**

# %%
concept_name <- "Sedative_Abuse"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Sedative Abuse")

# %% [markdown]
# ### **Sedative Dependence**

# %%
concept_name <- "Sedative_Dependence"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Sedative Dependence")

# %% [markdown]
# ### **Tobacco Use Disorder**

# %%
concept_name <- "Tobacco_Use_Disorder"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Tobacco Use Disorder")

# %%
concept_name <- "Tobacco_Use_Disorder"
table_type <- "Observation"
get_sql_concept(concept_name, table_type)
prep_save_observation_table(concept_name, "Tobacco Use Disorder")

# %%
concept_name <- "Tobacco_Use_Disorder"
table_type <- "Condition_Occurrence"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_co <- data_prep
table_type <- "Observation"
load(str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-{table_type}.rds"), verbose = F)
data_prep_o <- data_prep

data_prep <- bind_rows(data_prep_co, data_prep_o) %>%
    arrange(person_id, visit_occurrence_id, start_datetime)

data_prep %>% head

save(data_prep, file = str_glue("Data_Prepped/Prepped_Data-All_Participants-{concept_name}-Combined.rds"))
write.table(unique(data_prep$person_id), str_glue("Data_Prepped/IDs-{concept_name}-Combined.txt"), row.names = F, col.names = F)

# %% [markdown]
# ## Heart-Related Conditions

# %% [markdown]
# ### **Hypertension**
#
# *See above.*

# %% [markdown]
# ### **Hypercholesterolemia**
#
# *See above.*

# %% [markdown]
# ### **Hyperlipidemia**
#
# *See above.*

# %% [markdown]
# ### **Heart Disease**
#
# *See above.*

# %% [markdown]
# ### **Stroke**
#
# *See above.*

# %% [markdown]
# ### **Diseases of Arteries, Arterioles, and Capillaries**

# %%
concept_name <- "Diseases_of_the_Arteries_Artrioles_and_Capillaries"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Diseases of the Arteries Artrioles and Capillaries")

# %% [markdown]
# ### **Metabolic Syndrome and Other Insulin Resistance**

# %%
concept_name <- "Metabolic_Syndrome_and_Other_Insulin_Resistance"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Metabolic Syndrome and Other Insulin Resistance")

# %% [markdown]
# ## Healthcare Utilization

# %% [markdown]
# ### **Antidepressant prescriptions**
#
# *See above.*

# %% [markdown]
# ### **Overall Medical Visits**
#
# *See above.*

# %% [markdown]
# ### **Outpatient Medical Visits**
#
# *See above.*

# %% [markdown]
# ## Psychotropic Drugs (Excluding Antidepressants)

# %% [markdown]
# ### Antipsychotics

# %%
concept_name <- "Antipsychotics"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, verbose = F)
translate_table(concept_name, table_type, drug_category_file = "Drug_ATC_Categories.csv", drug_category_filter_to = "ANTIPSYCHOTICS", 
                drug_specific_exclusions = "lithium")

# %% [markdown]
# ### Anxiolytics

# %%
concept_name <- "Anxiolytics"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, verbose = F)
translate_table(concept_name, table_type, drug_category_file = "Drug_ATC_Categories.csv", drug_category_filter_to = "ANXIOLYTICS")

# %% [markdown]
# ### Hypnotics and Sedatives

# %%
concept_name <- "Hypnotics_and_Sedatives"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, "Hypnotics and Sedatives", verbose = F)
translate_table(concept_name, table_type, drug_category_file = "Drug_ATC_Categories.csv", drug_category_filter_to = "HYPNOTICS AND SEDATIVES")

# %% [markdown]
# ### Psychostimulants, Agents used for ADHD and Nootropics

# %%
concept_name <- "Psychostimulants_Agents_used_for_ADHD_and_Nootropics"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, "Psychostimulants", verbose = F)
translate_table(concept_name, table_type, drug_category_file = "Drug_ATC_Categories.csv", drug_category_filter_to = "PSYCHOSTIMULANTS, AGENTS USED FOR ADHD AND NOOTROPICS")

# %% [markdown]
# ### Psycholeptics and Psychoanaleptics in Combination

# %%
concept_name <- "Psycholeptics_and_Psychoanaleptics_in_Combination"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, "Psycholeptics and Psychoanaleptics in Combination", verbose = F)
translate_table(concept_name, table_type, drug_category_file = "Drug_ATC_Categories.csv")

# %% [markdown]
# ### Anti-Dementia Drugs

# %%
concept_name <- "Anti_Dementia_Drugs"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, "Anti-Dementia Drugs", verbose = F)
translate_table(concept_name, table_type, drug_category_file = "Drug_ATC_Categories.csv", drug_category_filter_to = "ANTI-DEMENTIA DRUGS")

# %% [markdown]
# # Other

# %%
concept_name <- "Bipolar_Disorder"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, "Bipolar Disorder", verbose = F)

# %%
concept_name <- "Schizophrenia"
table_type <- "Condition_Occurrence"
get_sql_concept(concept_name, table_type)
prep_save_condition_table(concept_name, verbose = F)

# %%
concept_name <- "GLP_1RAs"
concept_name_simp <- "GLP-1RAs"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, concept_name_simp)

# %%
concept_name <- "Antidiabetics"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name)

# %%
concept_name <- "Antidiabetics_Excluding_GLP_1RAs"
concept_name_simp <- "Antidiabetics excluding GLP-1RAs"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, concept_name_simp)

# %%
concept_name <- "Antidiabetics_Excluding_Semaglutide"
concept_name_simp <- "Antidiabetics excluding Semaglutide"
table_type <- "Drug_Exposure"
get_sql_concept(concept_name, table_type)
prep_save_drug_table(concept_name, concept_name_simp)

# %%
