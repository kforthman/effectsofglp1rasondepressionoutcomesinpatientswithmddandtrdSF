# -*- coding: utf-8 -*-
# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
concept_name <- "Mood_Disorders"
table_type <- "Condition_OccurrenceUVisit"

# This query represents dataset "Mood Disorders" for domain "condition" and was generated for All of Us Controlled Tier Dataset v8
sql_query <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        v.visit_start_datetime,
        v.visit_end_datetime,
        CAST(v.visit_concept_id AS STRING) AS visit_concept_id,
        visit.concept_name AS visit_concept_name,
        visit.concept_code AS visit_concept_code,
        visit.vocabulary_id AS visit_concept_vocabulary,
        CAST(v.visit_type_concept_id AS STRING) AS visit_type_concept_id,
        visit_type.concept_name AS visit_type_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_source_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (1568209, 1568211, 1568213, 1568214, 1568217, 1568218, 1568219, 35207142, 35207143, 35207144, 35207145, 35207146, 35207147, 35207148, 35207149, 35207150, 35207151, 35207152, 35207153, 35207154, 35207155, 35207156, 35207157, 35207158, 35207159, 35207160, 35207161, 35207162, 35207163, 35207164, 35207165, 35207166, 37200319, 37200320, 37200321, 37200322, 44819539, 44819540, 44819541, 44819542, 44819545, 44820717, 44820718, 44820719, 44820720, 44820721, 44820722, 44820996, 44821820, 44821821, 44822972, 44822973, 44822974, 44822975, 44822976, 44822977, 44822978, 44822979, 44822980, 44822981, 44822982, 44822983, 44824114, 44824115, 44824116, 44824117, 44824118, 44825292, 44825293, 44825299, 44826498, 44826499, 44826500, 44826501, 44827653, 44827654, 44827655, 44827656, 44827657, 44828830, 44828831, 44829922, 44829923, 44829924, 44829925, 44831087, 44831088, 44831089, 44831090, 44831091, 44831092, 44831095, 44832231, 44832232, 44832233, 44832234, 44832235,
 44832236, 44833400, 44833421, 44834589, 44834590, 44835784, 44835785, 44835786, 44836964, 44836965, 44836966, 44836972, 45538061, 45538062, 45538063, 45538064, 45542838, 45542839, 45542841, 45542842, 45542843, 45542844, 45547703, 45547704, 45547706, 45552477, 45552479, 45557200, 45557201, 45566839, 45566840, 45566841, 45571753, 45571756, 45571761, 45576538, 45576539, 45576540, 45576541, 45581455, 45586235, 45591134, 45591135, 45591136, 45595901, 45595903, 766349)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 0 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` visit_type 
            ON v.visit_type_concept_id = visit_type.concept_id
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

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
    condition_concept_id = col_character(),
    standard_concept_name = col_character(),
    standard_concept_code = col_character(),
    standard_vocabulary = col_character(),
    condition_start_datetime = col_character(),
    condition_end_datetime = col_character(),
    condition_type_concept_id = col_character(),
    condition_type_concept_name = col_character(),
    stop_reason = col_character(),
    visit_occurrence_id = col_character(),
    visit_start_datetime = col_character(),
    visit_end_datetime = col_character(),
    visit_concept_id = col_character(),
    visit_concept_name = col_character(),
    visit_concept_code = col_character(),
    visit_concept_vocabulary = col_character(),
    visit_type_concept_id = col_character(),
    visit_type_concept_name = col_character(),
    condition_source_value = col_character(),
    condition_source_concept_id = col_character(),
    source_concept_name = col_character(),
    source_concept_code = col_character(),
    source_vocabulary = col_character(),
    condition_status_source_value = col_character(),
    condition_status_concept_id = col_character(),
    condition_status_concept_name = col_character()
)


md_visit_data <- bind_rows(
    map(system2('gsutil', args = c('ls', file_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
            message(str_glue('Loading {csv}.'))
            chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
            if (is.null(col_types)) {
                col_types <- spec(chunk)
            }
            chunk
        }))



md_visit_data %>% head %>% print_all_cols

# %%
md_visit_data %>%
count(visit_concept_name) %>%
arrange(-n) %>%
mutate(n = comma(n))

# %%
md_visit_data %>%
count(condition_type_concept_name) %>%
arrange(-n) %>%
mutate(n = comma(n))

# %%
er_mood_primary_visit_data <- md_visit_data %>%
filter(visit_concept_name %in% c(
    "Emergency Room Visit",
    "Emergency Room and Inpatient Visit",
    "Observation Room"
)) %>%
filter(condition_type_concept_name %in% c(
    "Primary Condition",
    "Primary diagnosis"
))

er_mood_primary_visit_data %>% nrow %>% comma
er_mood_primary_visit_data %>% head

# %%
drug_matchedDS_info <- c("Insulins", "PS_Matched_Dataset-Insulins.rds",
                        "Metformin", "PS_Matched_Dataset-Metformin.rds",
                        "DPP4i", "PS_Matched_Dataset-DPP4i.rds",
                        "SGLT2i", "PS_Matched_Dataset-SGLT2i.rds",
                        "SU", "PS_Matched_Dataset-SU.rds",
                        "TZD", "PS_Matched_Dataset-TZD.rds",
                        "GLP1RA", "PS_Matched_Dataset-GLP1RA.rds"
                        ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug", "file_path"))

drug_matchedDS_info

# %%
for(i in 1:nrow(drug_matchedDS_info)){
    paste0("Loading file \"", drug_matchedDS_info$file_path[i], "\"\n") %>% cat
    load(drug_matchedDS_info$file_path[i])

    matched.data <- matched.data %>%
    mutate(first_drug_record = if_else(treatment_name == "Semaglutide", 
                                       Semaglutide_first_drug_record, 
                                       !!sym(paste0(drug_matchedDS_info$drug[i], "_first_drug_record")))) %>%
    dplyr::select(person_id) %>%
    mutate(study_cohort = paste0("Semaglutide vs ", drug_matchedDS_info$drug[i]))


    var_name <- paste0("dte_cohort_ids_", drug_matchedDS_info$drug[i])
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, matched.data)
}

# %%
dte_cohort_ids_allSets <- rbind(
    dte_cohort_ids_Insulins,
    dte_cohort_ids_Metformin,
    dte_cohort_ids_DPP4i,
    dte_cohort_ids_SGLT2i,
    dte_cohort_ids_SU,
    dte_cohort_ids_TZD,
    dte_cohort_ids_GLP1RA
)

dte_cohort_ids_allSets <- dte_cohort_ids_allSets %>% mutate(study_cohort = as.factor(study_cohort))

dte_cohort_ids_allSets %>% head(10)

dte_cohort_ids_allSets %>% dim

dte_cohort_ids_allSets$study_cohort %>% table()

# %%
my_histogram <- function(data, var, title, subtitle = "", xlab, ylab, ylog = F, xscalefactor = 1, labscalefactor = 1){
    hist_data <- data.frame(hist_var = data %>% pull(!!sym(var)))

    n_val <- max(hist_data$hist_var)
    int_val <- 10^(nchar(as.character(n_val)) - 2)*xscalefactor
    max_val <- ceiling(n_val/int_val)*int_val

    if(ylog){
        ggplot(hist_data, aes(hist_var)) +
        geom_histogram(aes(y = after_stat(count + 1)),
                       breaks = seq(0, max_val, by = int_val),
                       fill = "steelblue",
                       color = "white") +
        geom_text(
            stat    = "bin",
            breaks  = seq(0, max_val, by = int_val),
            aes(
                label = comma(after_stat(count)), 
                y     = after_stat((count + 1) * 1.05)     # 1.05× puts it just above
            ),
            size = 7*labscalefactor,
            vjust = 0, hjust = 0.25
        ) +
        scale_x_continuous(
            breaks = seq(0, max_val, by = int_val),
            expand = c(0.01, 0)
        ) +
        scale_y_log10() +
        annotation_logticks(sides = "l") +
        labs(
            title    = title,
            subtitle = subtitle,
            x        = xlab,
            y        = ylab,
        )  +
        theme_minimal(base_size = 25*labscalefactor) 
    }else{
        ggplot(hist_data, aes(hist_var)) +
        geom_histogram(breaks = seq(0, max_val, by = int_val),
                       fill = "steelblue",
                       color = "white") +
        geom_text(
            stat    = "bin",
            breaks  = seq(0, max_val, by = int_val),
            aes(
                label = comma(after_stat(count)), 
                y     = after_stat(count)     # 1.05× puts it just above
            ),
            size = 7*labscalefactor,
            vjust = 0, hjust = 0.25
        ) +
        scale_x_continuous(
            breaks = seq(0, max_val, by = int_val),
            expand = c(0.01, 0)
        ) +
        labs(
            title    = title,
            subtitle = subtitle,
            x        = xlab,
            y        = ylab,
        )  +
        theme_minimal(base_size = 25*labscalefactor)
    }
}

# %%
