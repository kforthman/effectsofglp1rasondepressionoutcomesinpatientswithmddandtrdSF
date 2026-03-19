# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
depression_ds <- grab_data("    SELECT
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
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name,
        c_ext.src_id as src_id
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
                        concept_id IN (1568217, 1568218, 1568219, 35207150, 35207151, 35207152, 35207153, 35207154, 35207155, 35207156, 35207157, 35207158, 35207159, 35207160, 35207161, 35207163, 37200319, 37200320, 44820717, 44821821, 44822975, 44822976, 44824114, 44825293, 44827654, 44827655, 44827656, 44829923, 44831089, 44831090, 44832232, 44833421, 44834589, 44835784, 44835785, 44836972, 45547704, 45547706, 45552479, 45571761, 45576541, 766349)       
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
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id
    LEFT JOIN 
        `condition_occurrence_ext` c_ext
            ON c_occurrence.condition_occurrence_id = c_ext.condition_occurrence_id
")
depression_ds %>% 
filter(!str_starts(src_id, "EHR site")) %>%
arrange(person_id) %>%
head %>%
print_all_cols

# %%
depression_no_EHR <- depression_ds %>%
mutate(EHR_record = ifelse(str_starts(src_id, "EHR site"), "EHR_record", "not_EHR_record"))%>% 
count(person_id, EHR_record) %>%
pivot_wider(names_from = EHR_record, values_from = n, values_fill = 0) %>%
filter(EHR_record == 0, not_EHR_record > 0)

depression_no_EHR %>% nrow
depression_no_EHR

# %%
depression_ds %>% pull(person_id) %>% unique %>% length

# %%

# %%
