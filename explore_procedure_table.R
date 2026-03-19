# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
procedure_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `procedure_occurrence`
LIMIT 500;
")
procedure_occurrence_data %>% head
procedure_occurrence_data %>% colnames %>% paste(collapse = ", \n\t") %>% cat

# %%
procedure_occurrence_data <- grab_data("
SELECT 
    CAST(procedure_occurrence_id AS STRING) AS procedure_occurrence_id_str,
    person_id,
    procedure_concept_id, 
    procedure_date, 
    procedure_datetime, 
    procedure_type_concept_id, 
    CAST(modifier_concept_id AS STRING) AS modifier_concept_id_str,
    quantity, 
    CAST(provider_id AS STRING) AS provider_id_str, 
    CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id_str, 
    CAST(visit_detail_id AS STRING) AS visit_detail_id_str, 
    procedure_source_value, 
    procedure_source_concept_id, 
    CAST(modifier_source_value AS STRING) AS modifier_source_value_str
FROM
    `procedure_occurrence`
LIMIT 500;
")
procedure_occurrence_data %>% head

# %%
procedure_occurrence_data <- grab_data("
SELECT COUNT(*) AS row_count
FROM `procedure_occurrence`;")
"\tN Rows: " %>% cat
procedure_occurrence_data[[1]] %>% comma %>% cat

procedure_occurrence_data <- grab_data("
SELECT  COUNT(*) AS column_count
FROM INFORMATION_SCHEMA.COLUMNS
WHERE table_name='procedure_occurrence'
")
"\n\tN Columns: " %>% cat
procedure_occurrence_data[[1]] %>% comma %>% cat

# %%
col_name <- "procedure_occurrence_id"
col_num <- 1

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
col_name <- "person_id"
col_num <- 2

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
col_name <- "procedure_concept_id"
col_num <- 3

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT procedure_occurrence.procedure_occurrence_id) AS count_records
FROM
    `procedure_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON procedure_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
procedure_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "procedure_date"
col_num <- 4

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
col_name <- "procedure_datetime"
col_num <- 5

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
col_name <- "procedure_type_concept_id"
col_num <- 6

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT procedure_occurrence.procedure_occurrence_id) AS count_records
FROM
    `procedure_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON procedure_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
procedure_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "modifier_concept_id"
col_num <- 7

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT procedure_occurrence.procedure_occurrence_id) AS count_records
FROM
    `procedure_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON procedure_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
procedure_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "quantity"
col_num <- 8

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
col_name <- "provider_id"
col_num <- 9

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
col_name <- "visit_occurrence_id"
col_num <- 10

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT procedure_occurrence.procedure_occurrence_id) AS count_records
FROM
    `procedure_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON procedure_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
procedure_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "visit_detail_id"
col_num <- 11

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT procedure_occurrence.procedure_occurrence_id) AS count_records
FROM
    `procedure_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON procedure_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
procedure_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "procedure_source_value"
col_num <- 12

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
col_name <- "procedure_source_concept_id"
col_num <- 13

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT procedure_occurrence.procedure_occurrence_id) AS count_records
FROM
    `procedure_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON procedure_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
LIMIT 50
"))
"\n\nunique value counts:\n" %>% cat
procedure_occurrence_summ %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "modifier_source_value"
col_num <- 14

"col {col_num} | {col_name}" %>% str_glue %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `procedure_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `procedure_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
procedure_occurrence_summ[[1]] %>% comma %>% cat

procedure_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `procedure_occurrence`;
"))
"\n\thead (10): " %>% cat
procedure_occurrence_summ[[1]] %>% cat

# %%
