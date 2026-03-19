# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
visit_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `condition_occurrence`
LIMIT 500;
")
visit_occurrence_data %>% head
visit_occurrence_data %>% colnames %>% paste(collapse = ", \n\t") %>% cat

# %%
visit_occurrence_data <- grab_data("
SELECT 
    CAST(condition_occurrence_id AS STRING) AS condition_occurrence_id_str,
    person_id,
    condition_concept_id, 
    condition_start_date, 
    condition_start_datetime, 
    condition_end_date, 
    condition_end_datetime, 
    condition_type_concept_id, 
    CAST(condition_status_concept_id AS STRING) AS condition_status_concept_id_str,
    stop_reason, 
    provider_id, 
    CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
    CAST(visit_detail_id AS STRING) AS visit_detail_id_str,
    condition_source_value, 
    CAST(condition_source_concept_id AS STRING) AS condition_source_concept_id_str,
    condition_status_source_value
FROM
    `condition_occurrence`
LIMIT 500;
")
visit_occurrence_data %>% head

# %%
condition_occurrence_data <- grab_data("
SELECT COUNT(*) AS row_count
FROM `condition_occurrence`;")
"\tN Rows: " %>% cat
condition_occurrence_data[[1]] %>% comma %>% cat

condition_occurrence <- grab_data("
SELECT  COUNT(*) AS column_count
FROM INFORMATION_SCHEMA.COLUMNS
WHERE table_name='condition_occurrence'
")
"\n\tN Columns: " %>% cat
condition_occurrence_data[[1]] %>% comma %>% cat

# %%
col_name <- "condition_occurrence_id"
col_num <- 1
"col {col_num} | {col_name}" %>% str_glue %>% cat
condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "person_id"
col_num <- 2
"col {col_num} | {col_name}" %>% str_glue %>% cat
condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "condition_concept_id"
col_num <- 3

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT condition_occurrence.condition_occurrence_id) AS count_records
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
condition_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "condition_start_date"
col_num <- 4

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tminimum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tmaximum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "condition_start_datetime"
col_num <- 5

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tminimum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tmaximum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "condition_end_date"
col_num <- 6

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tminimum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tmaximum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "condition_end_datetime"
col_num <- 7

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tminimum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `condition_occurrence`;
"))
"\n\tmaximum: " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "condition_type_concept_id"
col_num <- 8

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_id AS standard_concept_id,
    standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT condition_occurrence.condition_occurrence_id) AS count_records
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_id, standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
condition_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "condition_status_concept_id"
col_num <- 9

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name,
    COUNT(DISTINCT condition_occurrence.condition_occurrence_id) AS count_records
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
condition_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")


# %%
col_name <- "stop_reason"
col_num <- 10

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "provider_id"
col_num <- 11

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "visit_occurrence_id"
col_num <- 12

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "visit_detail_id"
col_num <- 13

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "condition_source_value"
col_num <- 14

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

# %%
col_name <- "condition_source_concept_id"
col_num <- 15

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT condition_occurrence.condition_occurrence_id) AS count_records
FROM
    `condition_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON condition_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
condition_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")


# %%
col_name <- "condition_status_source_value"
col_num <- 16

"col {col_num} | {col_name}" %>% str_glue %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `condition_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `condition_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
condition_occurrence_summ[[1]] %>% comma %>% cat

condition_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `condition_occurrence`;
"))
"\n\thead (10): " %>% cat
condition_occurrence_summ[[1]] %>% cat
