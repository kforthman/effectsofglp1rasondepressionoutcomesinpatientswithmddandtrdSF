# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
col_name <- "visit_concept_id"
col_num <- 3

# Heading
"col {col_num} | {col_name}" %>% str_glue %>% cat

# Number of unique values
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

# Number of blank values
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

# First 10 values
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

# All unique values and their frequency
concept_id_table <- grab_data("
SELECT 
    visit_occurrence.visit_concept_id AS standard_concept_id,
    standard_concept.concept_name AS standard_concept_name, 
    standard_concept.domain_id AS concept_domain,
    standard_concept.vocabulary_id AS concept_vocabulary,
    standard_concept.concept_class_id AS concept_class_id,
    standard_concept.standard_concept AS concept_is_standard,
    standard_concept.concept_code AS concept_code,
    COUNT(DISTINCT visit_occurrence.visit_occurrence_id) AS count_records
FROM
    `visit_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON visit_occurrence.visit_concept_id = standard_concept.concept_id
LEFT JOIN
    `visit_occurrence_ext` v_ext
    ON visit_occurrence.visit_occurrence_id = v_ext.visit_occurrence_id
WHERE LOWER(v_ext.src_id) LIKE 'ehr site%'
GROUP BY standard_concept_id, standard_concept_name, concept_domain, concept_vocabulary, concept_class_id, concept_is_standard, concept_code
ORDER BY count_records DESC
")
"\n\nunique value counts:\n" %>% cat
concept_id_table %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
# Tier 1 - Inpatient and ICU
tier_1_table <- concept_id_table %>% filter(grepl("inpatient|hospital|rehab|icu|ambulance|intensive care|hospice|alternate care site", standard_concept_name, ignore.case = T) & 
                                            !grepl("non-hospital|outpatient|Emergency Room - Hospital|ambulatory", standard_concept_name, ignore.case = T))
tier_1_names <- tier_1_table %>% pull(standard_concept_name)
tier_1_table

# %%
# Tier 2 - Emergency or Urgent Care
tier_2_table <- concept_id_table %>% filter(grepl("emergency|urgent|observation room", standard_concept_name, ignore.case = T) & !grepl("non-emergency|inpatient", standard_concept_name, ignore.case = T))
tier_2_names <- tier_2_table %>% pull(standard_concept_name)
tier_2_table

# %%
# Tier 3 - Specialized Outpatient/Parial Hospital
tier_3_table <- concept_id_table %>% filter(grepl("outpatient hospital|ambulatory rehabilitation visit|assisted living|nursing facility", standard_concept_name, ignore.case = T) & !grepl("non-medical", standard_concept_name, ignore.case = T))
tier_3_names <- tier_3_table %>% pull(standard_concept_name)
tier_3_table

# %%
# Tier 4 - General Outpatient Visits
tier_4_table <- concept_id_table %>% filter(grepl("office visit|outpatient visit|outpatient rehabilitation|ambulatory|telehealth|home visit|home health|case management visit|health exam|therapy|treatment facility|mammography center|health center", standard_concept_name, ignore.case = T)& 
                                            !grepl("ambulatory rehabilitation visit", standard_concept_name, ignore.case = T))
tier_4_names <- tier_4_table %>% pull(standard_concept_name)
tier_4_table

# %%
# Tier 5 - Unclear/Low relevance
tier_5_table <- concept_id_table %>% filter(grepl("pharmacy|mass immunization center|non-hospital institution|unknown|Laboratory Visit|other place of service|metabolic panel|blood bank|non-emergency medical transport|patient encounter procedure|comprehensive preventive medicine|drug test|counseling", standard_concept_name, ignore.case = T))
tier_5_names <- tier_5_table %>% pull(standard_concept_name)
tier_5_table

# %%
# Tier X - Exclude these visits
tier_X_table <- concept_id_table %>% filter(grepl("taxi|place of employment|bus|religious|No significant depressive symptoms as categorized by using a standardized depression assessment tool|Delayed hypersensitivity skin test for tuberculin PPD|1 ML hepatitis A vaccine|Body Mass Index|Medication list documented in medical record|Venipuncture for blood test", standard_concept_name, ignore.case = T))
tier_X_names <- tier_X_table %>% pull(standard_concept_name)
tier_X_table

# %%
concept_id_table_tiered <- concept_id_table %>% 
mutate(tier_1 = standard_concept_name %in% tier_1_names)%>% 
mutate(tier_2 = standard_concept_name %in% tier_2_names)%>% 
mutate(tier_3 = standard_concept_name %in% tier_3_names)%>% 
mutate(tier_4 = standard_concept_name %in% tier_4_names)%>% 
mutate(tier_5 = standard_concept_name %in% tier_5_names)%>%
mutate(tier_X = standard_concept_name %in% tier_X_names)%>%
rowwise %>%
mutate(dup = sum(across(tier_1:tier_X))) %>%
ungroup 

concept_id_table_tiered %>%
filter(dup > 1) %>% 
nrow

concept_id_table_tiered %>%
filter(dup < 1) %>% 
nrow

# %%
concept_id_table_tiered_short <- concept_id_table_tiered %>%
mutate(tier = ifelse(tier_1, 1, ifelse(tier_2, 2, ifelse(tier_3, 3, ifelse(tier_4, 4, ifelse(tier_5,5, NA)))))) %>%
dplyr::select(-tier_1, -tier_2, -tier_3, -tier_4, -tier_5, -tier_X, -dup) %>%
arrange(tier)

# %%
concept_id_table_tiered_short %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
write.csv(concept_id_table_tiered_short, "visit_type_tiers.csv")

# %%
visit_occurrence_data <- grab_data("
SELECT 
    CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
    person_id,
    visit_concept_id,
    visit_start_date,
    visit_start_datetime,
    visit_end_date,
    visit_end_datetime,
    visit_type_concept_id,
    CAST(provider_id AS STRING) AS provider_id_str,
    CAST(care_site_id AS STRING) AS care_site_id_str,
    CAST(visit_source_value AS STRING) AS visit_source_value_str,
    CAST(visit_source_concept_id AS STRING) AS visit_source_concept_id_str,
    CAST(admitting_source_concept_id AS STRING) AS admitting_source_concept_id_str,
    CAST(admitting_source_value AS STRING) AS admitting_source_value_str,
    CAST(discharge_to_concept_id AS STRING) AS discharge_to_concept_id_str,
    CAST(discharge_to_source_value AS STRING) AS discharge_to_source_value_str,
    CAST(preceding_visit_occurrence_id AS STRING) AS preceding_visit_occurrence_id_str
FROM
    `visit_occurrence`
LEFT JOIN
    `visit_occurrence_ext` v_ext
    ON visit_occurrence.visit_occurrence_id = v_ext.visit_occurrence_id
WHERE LOWER(v_ext.src_id) LIKE 'ehr site%'
WHERE visit_concept_id IN
")

# %%
