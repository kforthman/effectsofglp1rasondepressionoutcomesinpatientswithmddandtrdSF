# %%
# sql = """
#     -- Select *
#     Select condition_occurrence.person_id As person_id, concept.concept_code As ICD_code, concept.vocabulary_id as vocabulary, condition_occurrence.condition_start_date As start_date, condition_occurrence.condition_start_date As end_date
#     From `""" + os.environ["WORKSPACE_CDR"] + """.condition_occurrence` As condition_occurrence
#         Left Join `""" + os.environ["WORKSPACE_CDR"] + """.concept` As concept
#             On condition_occurrence.condition_source_concept_id = concept.concept_id
#     Where concept.vocabulary_id LIKE 'ICD10CM' OR concept.vocabulary_id LIKE 'ICD9CM'
#     Order By person_id, start_date, ICD_code
#     """

# icd = pd.read_gbq(sql, dialect="standard", use_bqstorage_api=("BIGQUERY_STORAGE_API_ENABLED" in os.environ), progress_bar_type="tqdm_notebook")
# icd.to_parquet("gs://fc-secure-778239c4-00dd-4ab9-8bc9-1463fd36d089/projects/TRD/v8/trd_revision/icd.parquet", index=False)
# pl.from_pandas(icd).unique().sort(["person_id", "start_date", "ICD_code"]).group_by("person_id", maintain_order=True).map_groups(lambda x: x.with_row_index()).pivot(on="index", index="person_id", values=["ICD_code", "start_date", "end_date"]).to_pandas().to_parquet("gs://fc-secure-778239c4-00dd-4ab9-8bc9-1463fd36d089/projects/TRD/v8/trd_revision/icd_wide.parquet", index=False)
# # 125953803

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
concept_data_limit <- grab_data("
SELECT 
    *
FROM
    `concept`
WHERE concept.vocabulary_id LIKE 'ICD10CM' OR concept.vocabulary_id LIKE 'ICD9CM'
LIMIT 500;
")
concept_data_limit %>% head
concept_data_limit %>% colnames %>% paste(collapse = ", \n\t") %>% cat

# %%
concept_data <- grab_data("
SELECT 
    *
FROM
    `concept`
")
concept_data %>% dim

# %%
concept_data %>% colnames

# %%
concept_data %>% pull(concept_id) %>% unique %>% length

# %%
concept_ICD_data <- grab_data("
SELECT 
    *
FROM
    `concept`
WHERE
    concept_id IN (1568217, 1568218, 1568219, 35207150, 35207151, 35207152, 35207153, 35207154, 35207155, 35207156, 35207157, 35207158, 35207159, 35207160, 35207161, 35207163, 37200319, 37200320, 44820717, 44821821, 44822975, 44822976, 44824114, 44825293, 44827654, 44827655, 44827656, 44829923, 44831089, 44831090, 44832232, 44833421, 44834589, 44835784, 44835785, 44836972, 45547704, 45547706, 45552479, 45571761, 45576541, 766349)
")
concept_ICD_data

# %%
concept_ICD_data <- grab_data("
SELECT 
    *
FROM
    `concept`
WHERE
    concept_id IN (1569124, 35207668, 35207675, 35207676, 35207677, 35207678, 35207679, 37200492, 37200493, 37200494, 37200495, 44821949, 44823109, 44824236, 44827781, 44830078, 44831234, 44832370, 44832371, 44833556, 44834715, 44835925, 44837098)
")
concept_ICD_data

# %%
concept_ICD_data <- grab_data("
SELECT 
    *
FROM
    `concept`
WHERE
    concept_name IN ('Essential hypertension')
")
concept_ICD_data

# %%
condition_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `condition_occurrence`
WHERE
    condition_concept_id IN (40323437, 3105646, 45932564, 320128, 44833556, 3124279)
")
condition_occurrence_data %>% head
condition_occurrence_data %>% dim

# %%
condition_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `condition_occurrence`
WHERE
    condition_concept_id IN (44833556)
")
condition_occurrence_data %>% head
condition_occurrence_data %>% dim

# %%
concept_ICD_data <- grab_data("
SELECT 
    *
FROM
    `concept`
WHERE
    concept_name IN ('semaglutide', 'Semaglutide')
")
concept_ICD_data

# %%
drug_exposure_data <- grab_data("
SELECT 
    *
FROM
    `drug_exposure`
WHERE
    drug_concept_id IN (35622516, 3198051, 36694880, 793143)
")
drug_exposure_data %>% head
drug_exposure_data %>% dim

# %%
condition_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `drug_exposure`
WHERE
    drug_concept_id IN (793143)
")
drug_exposure_data %>% head
drug_exposure_data %>% dim

# %%
concept_ICD_data <- grab_data("
SELECT 
    *
FROM
    `concept`
WHERE concept.vocabulary_id LIKE 'ICD10CM' OR concept.vocabulary_id LIKE 'ICD9CM'
")
concept_ICD_data %>% dim

# %%
condition_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `condition_occurrence`
LIMIT 500;
")
condition_occurrence_data %>% head
condition_occurrence_data %>% colnames %>% paste(collapse = ", \n\t") %>% cat

# %%
condition_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_concept_id = standard_concept.concept_id
LIMIT 500
")
condition_occurrence_data %>% head
condition_occurrence_data %>% colnames %>% paste(collapse = ", \n\t") %>% cat

# %%
col_name <- "vocabulary_id"

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_concept_id = standard_concept.concept_id
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_concept_id = standard_concept.concept_id
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_concept_id = standard_concept.concept_id
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT {col_name} AS {col_name}, 
    COUNT(DISTINCT condition_occurrence.condition_occurrence_id) AS count_records
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_concept_id = standard_concept.concept_id
GROUP BY {col_name}
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
condition_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
SELECT
    condition_occurrence.person_id AS person_id, 
    concept.concept_code AS ICD_code, 
    concept.vocabulary_id AS vocabulary, 
    condition_occurrence.condition_start_date AS start_date, 
    condition_occurrence.condition_start_date AS end_date
FROM
    `condition_occurrence`
LEFT JOIN
    `concept`
    ON condition_occurrence.condition_source_concept_id = concept.concept_id
#     Select condition_occurrence.person_id As person_id, concept.concept_code As ICD_code, concept.vocabulary_id as vocabulary, condition_occurrence.condition_start_date As start_date, condition_occurrence.condition_start_date As end_date
#     From `""" + os.environ["WORKSPACE_CDR"] + """.condition_occurrence` As condition_occurrence
#         Left Join `""" + os.environ["WORKSPACE_CDR"] + """.concept` As concept
#             On condition_occurrence.condition_source_concept_id = concept.concept_id
#     Where concept.vocabulary_id LIKE 'ICD10CM' OR concept.vocabulary_id LIKE 'ICD9CM'
#     Order By person_id, start_date, ICD_code

# %%
col_name <- "vocabulary_id"

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_source_concept_id = standard_concept.concept_id
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_source_concept_id = standard_concept.concept_id
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_source_concept_id = standard_concept.concept_id
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT {col_name} AS {col_name}, 
    COUNT(DISTINCT condition_occurrence.condition_occurrence_id) AS count_records
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_source_concept_id = standard_concept.concept_id
GROUP BY {col_name}
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
condition_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
condition_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT concept_name AS concept_name, 
    COUNT(DISTINCT condition_occurrence.condition_occurrence_id) AS count_records
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.condition_source_concept_id = standard_concept.concept_id
WHERE vocabulary_id IN ('SNOMED')
GROUP BY concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
condition_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
cb_criteria_data <- grab_data("
SELECT 
    *
FROM
    `cb_criteria`
WHERE
                                      concept_id IN (1568217, 1568218, 1568219, 35207150, 35207151, 35207152, 35207153, 35207154, 35207155, 35207156, 35207157, 35207158, 35207159, 35207160, 35207161, 35207163, 37200319, 37200320, 44820717, 44821821, 44822975, 44822976, 44824114, 44825293, 44827654, 44827655, 44827656, 44829923, 44831089, 44831090, 44832232, 44833421, 44834589, 44835784, 44835785, 44836972, 45547704, 45547706, 45552479, 45571761, 45576541, 766349)
LIMIT 500
")
cb_criteria_data %>% head
cb_criteria_data %>% colnames %>% paste(collapse = ", \n\t") %>% cat



# %%
#     -- Select *
#     Select condition_occurrence.person_id As person_id, concept.concept_code As ICD_code, concept.vocabulary_id as vocabulary, condition_occurrence.condition_start_date As start_date, condition_occurrence.condition_start_date As end_date
#     From `""" + os.environ["WORKSPACE_CDR"] + """.condition_occurrence` As condition_occurrence
#         Left Join `""" + os.environ["WORKSPACE_CDR"] + """.concept` As concept
#             On condition_occurrence.condition_source_concept_id = concept.concept_id
#     Where concept.vocabulary_id LIKE 'ICD10CM' OR concept.vocabulary_id LIKE 'ICD9CM'
#     Order By person_id, start_date, ICD_code
#     """
