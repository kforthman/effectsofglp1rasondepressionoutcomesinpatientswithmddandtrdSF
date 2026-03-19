# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %% [markdown]
# intensive outpatient program (IOP)
# vs
# inpatient
#
# ask hd about missing visits

# %%
concept_name <- "Psychologic_or_Psychiatric_Visits_v2"
table_type <- "ProcedureUVisit"

sql_query <- ("
    SELECT
        CAST(procedure.person_id AS STRING) AS person_id,
        CAST(procedure.procedure_concept_id AS STRING) AS procedure_concept_id,
        p_standard_concept.concept_name AS procedure_concept_name,
        p_standard_concept.concept_code AS procedure_concept_code,
        p_standard_concept.vocabulary_id AS procedure_concept_vocabulary,
        procedure.procedure_datetime,
        v.visit_start_datetime,
        v.visit_end_datetime,
        CAST(procedure.procedure_type_concept_id AS STRING) AS procedure_type_concept_id,
        p_type.concept_name AS procedure_type_concept_name,
        CAST(procedure.modifier_concept_id AS STRING) AS procedure_modifier_concept_id,
        p_modifier.concept_name AS procedure_modifier_concept_name,
        procedure.quantity AS procedure_quantity,
        CAST(procedure.visit_occurrence_id AS STRING) AS visit_occurrence_id,
        CAST(v.visit_concept_id AS STRING) AS visit_concept_id,
        p_visit.concept_name AS visit_concept_name,
        p_visit.concept_code AS visit_concept_code,
        p_visit.vocabulary_id AS visit_concept_vocabulary,
        CAST(v.visit_type_concept_id AS STRING) AS visit_type_concept_id,
        p_visit_type.concept_name AS visit_type_concept_name,
        procedure.procedure_source_value,
        CAST(procedure.procedure_source_concept_id AS STRING) AS procedure_source_concept_id,
        p_source_concept.concept_name AS procedure_source_concept_name,
        p_source_concept.concept_code AS procedure_source_concept_code,
        p_source_concept.vocabulary_id AS procedure_source_concept_vocabulary,
        procedure.modifier_source_value AS procedure_modifier_source_value
    FROM
        ( SELECT
            * 
        FROM
            `procedure_occurrence` procedure 
        WHERE
            (
                procedure_source_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (2213518, 2213519, 2213520, 2213521, 2213522, 2213523, 2213524, 2213525, 2213526, 2213527, 2213528, 2213529, 2213530, 2213531, 2213532, 2213533, 2213534, 2213535, 2213536, 2213537, 2213538, 2213539, 2213540, 2213542, 2213543, 2213544, 2213545, 2213546, 2213547, 2213548, 2213549, 2213550, 2213551, 2213552, 2213554, 2213555, 2213556, 2213557, 2213558, 2213559, 2213560, 2213561, 40756837, 40757109, 42742381, 43527904, 43527905, 43527983, 43527984, 43527985, 43527986, 43527987, 43527988, 43527989, 43527990, 43527991, 43527992)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 0 
                    AND is_selectable = 1)
            )) procedure 
    LEFT JOIN
        `concept` p_standard_concept 
            ON procedure.procedure_concept_id = p_standard_concept.concept_id 
    LEFT JOIN
        `concept` p_type 
            ON procedure.procedure_type_concept_id = p_type.concept_id 
    LEFT JOIN
        `concept` p_modifier 
            ON procedure.modifier_concept_id = p_modifier.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON procedure.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` p_visit 
            ON v.visit_concept_id = p_visit.concept_id 
    LEFT JOIN
        `concept` p_visit_type 
            ON v.visit_type_concept_id = p_visit_type.concept_id
    LEFT JOIN
        `concept` p_source_concept 
            ON procedure.procedure_source_concept_id = p_source_concept.concept_id
")

file_path <- file.path(
    Sys.getenv("WORKSPACE_BUCKET"),
    "bq_exports",
    Sys.getenv("OWNER_EMAIL"),
    str_glue("All_Participants-{concept_name}-{table_type}-*.csv"))
message(str_glue('The data will be written to {file_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

gs_delete(file.path(
    "bq_exports",
    Sys.getenv("OWNER_EMAIL"),
    str_glue("All_Participants-{concept_name}-{table_type}-*.csv")), 
          Sys.getenv("WORKSPACE_BUCKET"))

bq_table_save(
    bq_dataset_query(
        Sys.getenv("WORKSPACE_CDR"), 
        sql_query, 
        billing = Sys.getenv("GOOGLE_PROJECT")
    ),
    file_path,
    destination_format = "CSV")

col_types <- cols(
    person_id = col_character(),
    procedure_concept_id = col_character(),
    procedure_concept_name = col_character(),
    procedure_concept_code = col_character(),
    procedure_concept_vocabulary = col_character(),
    procedure_datetime = col_character(),
    visit_start_datetime = col_character(),
    visit_end_datetime = col_character(),
    procedure_type_concept_id = col_character(),
    procedure_type_concept_name = col_character(),
    procedure_modifier_concept_id = col_character(),
    procedure_modifier_concept_name = col_character(),
    procedure_quantity = col_character(),
    visit_occurrence_id = col_character(),
    visit_concept_id = col_character(),
    visit_concept_name = col_character(),
    visit_concept_code = col_character(),
    visit_concept_vocabulary = col_character(),
    visit_type_concept_id = col_character(),
    visit_type_concept_name = col_character(),
    procedure_source_value = col_character(),
    procedure_source_concept_id = col_character(),
    procedure_source_concept_name = col_character(),
    procedure_source_concept_code = col_character(),
    procedure_source_concept_vocabulary = col_character(),
    procedure_modifier_source_value = col_character()
)


po_visit_data <- bind_rows(
    map(system2('gsutil', args = c('ls', file_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
            message(str_glue('Loading {csv}.'))
            chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
            if (is.null(col_types)) {
                col_types <- spec(chunk)
            }
            chunk
        }))



po_visit_data %>% head %>% print_all_cols

# %%
po_visit_data %>% pull(procedure_datetime) %>% is.na %>% sum

# %% [markdown]
# # Categorizing events as inpatient or outpatient

# %%
po_visit_data.2 <- po_visit_data %>% 
mutate(is_inpatient = ifelse(
    (procedure_concept_code %in% c(90816, 90817, 90818, 90819, 90821, 90822, 90823, 90824, 90826, 90828, 90829) | 
    procedure_source_concept_code %in% c(90816, 90817, 90818, 90819, 90821, 90822, 90823, 90824, 90826, 90828, 90829) |
    visit_concept_name %in% c("Inpatient Visit", "Inpatient Hospital", "Inpatient Psychiatric Facility", "Emergency Room and Inpatient Visit",
                                                 "Comprehensive Inpatient Rehabilitation Facility", "Psychiatric Hospital", 
                                                  "Behavioral Disturbances Assisted Living Facility", "Non-hospital institution Visit")
                                                 ) &
    !is.na(procedure_concept_code), 
    TRUE, FALSE)) %>%
mutate(is_outpatient = ifelse(
    (procedure_concept_code %in% c(90804, 90805, 90806, 90807, 90808, 90809, 90810, 90811, 90812, 90813, 90814, 90815) |
    procedure_source_concept_code %in% c(90804, 90805, 90806, 90807, 90808, 90809, 90810, 90811, 90812, 90813, 90814, 90815) |
    visit_concept_name %in% c("Outpatient Visit", "Laboratory Visit", "Office Visit", "Outpatient Hospital", "Telehealth",
                                                 "Ambulatory Clinic / Center", "Ambulatory Occupational Medicine Clinic / Center", "Ambulatory Physical Therapy Clinic / Center",
                                                 "Ambulatory Rehabilitation Visit", "Ambulatory Surgical Center", "Home Visit", "Case Management Visit") 
    ) & 
    !is.na(procedure_concept_code), 
    TRUE, FALSE))

# %%
n_total <- po_visit_data.2 %>% nrow
# Conflicts
this_n <- (po_visit_data.2$is_inpatient & po_visit_data.2$is_outpatient) %>% sum
cat("Categorized as inpatient and outpatient: \n")
cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
# Inpatient only
this_n <- (po_visit_data.2$is_inpatient & !po_visit_data.2$is_outpatient) %>% sum
cat("\n\nCategorized as inpatient only: \n")
cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
# Outpatient only
this_n <- (!po_visit_data.2$is_inpatient & po_visit_data.2$is_outpatient) %>% sum
cat("\n\nCategorized as outpatient only: \n")
cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
# Unclassified only
this_n <- (!po_visit_data.2$is_inpatient & !po_visit_data.2$is_outpatient) %>% sum
cat("\n\nUnknown visit type: \n")
cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))

# %% [markdown]
# # What do we know about events that were not categorized as inpatient or outpatient?

# %% [markdown]
# ## Visit concept name of uncategorized events

# %%
po_visit_data.2 %>% filter(!is_inpatient & !is_outpatient) %>% pull(visit_concept_name) %>% table(useNA = "always") %>% as.data.frame %>% arrange(-Freq)

# %% [markdown]
# ## Procedure concept name of uncategorized events

# %%
# Look at the number of codes around these visits to determine if inpatient
po_visit_data.2 %>% filter(!is_inpatient & !is_outpatient) %>% pull(procedure_concept_name) %>% table(useNA = "always") %>% as.data.frame %>% arrange(-Freq)

# %%
po_visit_data.2 %>% filter(is_inpatient & !is_outpatient) %>% pull(procedure_concept_name) %>% table(useNA = "always") %>% as.data.frame %>% arrange(-Freq)

# %% [markdown]
# ## Procedure source concept name of uncategorized events

# %%
po_visit_data.2 %>% filter(!is_inpatient & !is_outpatient) %>% pull(procedure_source_concept_name) %>% table(useNA = "always") %>% as.data.frame %>% arrange(-Freq)

# %%
po_visit_data.2 %>% 
mutate(procedure_concept_type = 
       ifelse(!is.na(procedure_concept_code), 
              ifelse(procedure_concept_code %in% c(90816, 90817, 90818, 90819, 90821, 90822, 90823, 90824, 90826, 90828, 90829), 
                     "Inpatient",
                     ifelse(procedure_concept_code %in% c(90804, 90805, 90806, 90807, 90808, 90809, 90810, 90811, 90812, 90813, 90814, 90815), 
                            "Outpatient", 
                            "Unknown"))
              ,"Unknown")) %>%
mutate(procedure_source_concept_type = 
       ifelse(!is.na(procedure_source_concept_code), 
              ifelse(procedure_source_concept_code %in% c(90816, 90817, 90818, 90819, 90821, 90822, 90823, 90824, 90826, 90828, 90829), 
                     "Inpatient",
                     ifelse(procedure_source_concept_code %in% c(90804, 90805, 90806, 90807, 90808, 90809, 90810, 90811, 90812, 90813, 90814, 90815), 
                            "Outpatient", 
                            "Unknown"))
              ,"Unknown")) %>%
mutate(visit_concept_type = 
       ifelse(!is.na(visit_concept_name), 
              ifelse(visit_concept_name %in% c("Inpatient Visit", "Inpatient Hospital", "Inpatient Psychiatric Facility", "Emergency Room and Inpatient Visit",
                                               "Comprehensive Inpatient Rehabilitation Facility", "Psychiatric Hospital", 
                                               "Behavioral Disturbances Assisted Living Facility", "Non-hospital institution Visit"), 
              "Inpatient",
              ifelse(visit_concept_name %in% c("Outpatient Visit", "Laboratory Visit", "Office Visit", "Outpatient Hospital", "Telehealth",
                                               "Ambulatory Clinic / Center", "Ambulatory Occupational Medicine Clinic / Center", "Ambulatory Physical Therapy Clinic / Center",
                                               "Ambulatory Rehabilitation Visit", "Ambulatory Surgical Center", "Home Visit", "Case Management Visit"), 
                     "Outpatient", 
                     "Unknown"))
       ,"Unknown"))%>%
filter(is_inpatient & is_outpatient) %>% 
dplyr::select(procedure_concept_name, procedure_concept_type, procedure_source_concept_name, procedure_source_concept_type, visit_concept_name, visit_concept_type) %>% 
table %>% 
as.data.frame %>% 
filter(Freq > 0) %>% 
arrange(-Freq)

# %% [markdown]
# # Within cohort

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
    dplyr::select(person_id) %>%
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
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i]) # matched.data

    index_ext <- ifelse(drug_matchedDS_info$drug[i] == "Nontreatment", "_index", "_first_drug_record")
    
    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], index_ext))))

    po_visit_data_this_drug <- po_visit_data.2 %>% 
    filter(person_id %in% matched.data$person_id) %>%
    left_join(matched.data %>% dplyr::select(person_id, treatment, treatment_name, first_drug_record), by = join_by(person_id == person_id)) %>%
    mutate(time_from_index_days = time_length(interval(first_drug_record, procedure_datetime), "days")) %>%
    mutate(time_from_index_months = time_length(interval(first_drug_record, procedure_datetime), "months")) %>%
    mutate(treatment_name = factor(treatment_name, levels = c("Semaglutide", drug_matchedDS_info$drug[i]))) %>%
    mutate(visit_n_days =  time_length(interval(visit_start_datetime, visit_end_datetime), "days") + 1) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))

    var_name <- paste0("po_visit_data_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, po_visit_data_this_drug)
    
    "\n" %>% cat
}

# %%
po_visit_data_Insulins %>% head

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    this_drug <- drug_matchedDS_info$drug[i]
    if(this_drug == "Semaglutide"){next}

    these_ids <- get(paste0("dte_cohort_ids_", this_drug))
    these_visits <- get(paste0("po_visit_data_", this_drug))
    these_visits <- these_visits %>% filter(time_from_index_days>=15 & time_from_index_days<=365)

    cat(paste0("\n\n\n\n", this_drug, "\n"))
    n_total <- these_visits %>% nrow
    # Conflicts
    this_n <- (these_visits$is_inpatient & these_visits$is_outpatient) %>% sum
    cat("\n\nTotal psych procedures categorized as inpatient and outpatient: \n")
    cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
    # Inpatient only
    this_n <- (these_visits$is_inpatient & !these_visits$is_outpatient) %>% sum
    cat("\n\nTotal psych procedures categorized as inpatient only: \n")
    cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
    # Outpatient only
    this_n <- (!these_visits$is_inpatient & these_visits$is_outpatient) %>% sum
    cat("\n\nTotal psych procedures categorized as outpatient only: \n")
    cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
    # Unclassified only
    this_n <- (!these_visits$is_inpatient & !these_visits$is_outpatient) %>% sum
    cat("\n\nTotal psych procedures with unknown visit type: \n")
    cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
    # Total individuals with inpatient visit
    this_n <- these_visits %>% filter(is_inpatient) %>% pull(person_id) %>% unique %>% length
    n_total <- these_ids %>% nrow
    cat("\n\nTotal individuals in cohort with an inpatient psych procedure: \n")
    cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
    # Total individuals with outpatient visit
    this_n <- these_visits %>% filter(is_outpatient) %>% pull(person_id) %>% unique %>% length
    cat("\n\nTotal individuals in cohort with an outpatient psych procedure: \n")
    cat(paste0(comma(this_n), " / ", comma(n_total), " (", percent(this_n/n_total, accuracy = 0.01), ")"))
}

# %% [markdown]
# # Other

# %%
stop()

# %%
po_visit_data.2 %>% filter(in_or_out == "Unknown" & is.na(visit_concept_name)) %>% head %>% print_all_cols()

# %%
po_visit_data.2 %>% filter(in_or_out == "Inpatient") %>% pull(procedure_source_concept_name) %>% table(useNA = "always") %>% as.data.frame %>% arrange(-Freq)

# %%
# Sometimes, the visit concept and the procedure concept indicate conflicting visit type.
po_visit_data.2 %>% 
filter(visit_concept_name == "Inpatient Visit") %>% 
filter(procedure_concept_name == "Outpatient Visit") %>% 
dplyr::select(person_id, procedure_concept_name, procedure_source_concept_id, procedure_source_concept_name, procedure_datetime, visit_concept_id, visit_concept_name) %>% 
print_all_cols

# %%
po_visit_data.2 %>% 
filter(procedure_concept_name == "Outpatient Visit") %>% 
dplyr::select(person_id, procedure_concept_name, procedure_source_concept_id, procedure_source_concept_name, procedure_datetime, visit_concept_id, visit_concept_name) %>% 
print_all_cols

# %%
po_visit_data.2 %>% filter(in_or_out == "Unknown" & is.na(visit_concept_name)) %>% pull(procedure_concept_name) %>% table(useNA = "always") %>% as.data.frame %>% arrange(-Freq)

# %%
po_visit_data.2 %>% filter(in_or_out == "Unknown")
