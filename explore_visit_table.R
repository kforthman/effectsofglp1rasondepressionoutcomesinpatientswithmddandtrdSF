# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %% [markdown]
# # **Visit table**

# %% [markdown]
# ### Initial exploration

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
LIMIT 500;
")
visit_occurrence_data %>% head

# %%
visit_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `condition_era`
LIMIT 500;
")
visit_occurrence_data %>% head

# %%
visit_occurrence_data <- grab_data("
SELECT 
    CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
    *
FROM
    `observation`
LIMIT 500;
")
visit_occurrence_data %>% head

# %%
visit_occurrence_data <- grab_data("
SELECT COUNT(*) AS row_count
FROM `visit_occurrence`;")
"\tN Rows: " %>% cat
visit_occurrence_data[[1]] %>% comma %>% cat

visit_occurrence_data <- grab_data("
SELECT  COUNT(*) AS column_count
FROM INFORMATION_SCHEMA.COLUMNS
WHERE table_name='visit_occurrence'
")
"\n\tN Columns: " %>% cat
visit_occurrence_data[[1]] %>% comma %>% cat

# %% [markdown]
# ### **Columns summary:**

# %% [markdown]
# **Visit occurence id**

# %%
col_name <- "visit_occurrence_id"
col_num <- 1
"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Person ID**

# %%
col_name <- "person_id"
col_num <- 2
"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Visit concept ID**

# %%
col_name <- "visit_concept_id"
col_num <- 3

"col {col_num} | {col_name}" %>% str_glue %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

data <- grab_data("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_occurrence.visit_occurrence_id) AS count_records
FROM
    `visit_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON visit_occurrence.visit_concept_id = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
")
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %% [markdown]
# **Visit start date**

# %%
col_name <- "visit_start_date"
col_num <- 4
"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tminimum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tmaximum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Visit start datetime**

# %%
col_name <- "visit_start_datetime"
col_num <- 5
"col {col_num} | {col_name}" %>% str_glue %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tminimum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tmaximum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Visit end date**

# %%
col_name <- "visit_end_date"
col_num <- 6
"col {col_num} | {col_name}" %>% str_glue %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tminimum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tmaximum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Visit end datetime**

# %%
col_name <- "visit_end_datetime"
col_num <- 7
"col {col_num} | {col_name}" %>% str_glue %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MIN(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tminimum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  MAX(CAST({col_name} AS STRING)) AS min_value,
FROM `visit_occurrence`;
"))
"\n\tmaximum: " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Visit type concept ID**

# %%
col_name <- "visit_type_concept_id"
col_num <- 8

"col {col_num} | {col_name}" %>% str_glue %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat


data <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_occurrence.visit_occurrence_id) AS count_records
FROM
    `visit_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON visit_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %% [markdown]
# **Provider ID**

# %%
col_name <- "provider_id"
col_num <- 9

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Care site ID**

# %%
col_name <- "care_site_id"
col_num <- 10

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Visit source value**

# %%
col_name <- "visit_source_value"
col_num <- 11

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(DISTINCT {col_name}, ', \\n\\t\\t')
FROM `visit_occurrence`;
"))
"\n\tunique values: " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Visit source concept ID**

# %%
col_name <- "visit_source_concept_id"
col_num <- 12

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

data <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_occurrence.visit_occurrence_id) AS count_records
FROM
    `visit_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON visit_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %% [markdown]
# **Admitting source concept ID**

# %%
col_name <- "admitting_source_concept_id"
col_num <- 13

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

data <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_occurrence.visit_occurrence_id) AS count_records
FROM
    `visit_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON visit_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %% [markdown]
# **Admitting source value**

# %%
col_name <- "admitting_source_value"
col_num <- 14

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(DISTINCT {col_name}, ', \\n\\t\\t')
FROM `visit_occurrence`;
"))
"\n\tunique values: " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Discharge to concept ID**

# %%
col_name <- "discharge_to_concept_id"
col_num <- 15

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

data <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_occurrence.visit_occurrence_id) AS count_records
FROM
    `visit_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON visit_occurrence.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %% [markdown]
# **Discharge to source value**

# %%
col_name <- "discharge_to_source_value"
col_num <- 16

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(DISTINCT {col_name}, ', \\n\\t\\t')
FROM `visit_occurrence`;
"))
"\n\tunique values: " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# **Preceding visit occurence ID**

# %%
col_name <- "preceding_visit_occurrence_id"
col_num <- 17

"col {col_num} | {col_name}" %>% str_glue %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_summ[[1]] %>% comma %>% cat

visit_occurrence_summ <- grab_data(str_glue("
SELECT
  STRING_AGG(CAST({col_name} AS STRING), ', \\n\\t\\t' LIMIT 10) AS first_10_values
FROM `visit_occurrence`;
"))
"\n\thead (10): " %>% cat
visit_occurrence_summ[[1]] %>% cat

# %% [markdown]
# ### **Count of visits per year by person**

# %%
visit_occurence_id_count_bypersonyear <- grab_data("
SELECT 
    person_id,
    EXTRACT(YEAR FROM visit_start_date) AS visit_year,
    COUNT(DISTINCT CAST(visit_occurrence_id AS STRING)) AS count_ids
FROM
    `visit_occurrence`
WHERE
    visit_occurrence_id IS NOT NULL
    AND visit_start_date IS NOT NULL
GROUP BY 
    person_id,
    visit_year
ORDER BY 
    person_id ASC,
    visit_year ASC
LIMIT 500;")
visit_occurence_id_count_bypersonyear

# %% [markdown]
# ### Other Visit Tables

# %%
data_info_ncol <- grab_data("SELECT table_name, COUNT(DISTINCT column_name) count_cols
          FROM INFORMATION_SCHEMA.COLUMNS
          GROUP BY table_name
          ORDER BY table_name")
apply(data_info_ncol, 1, function(x){paste(str_pad(x[1], 28, "right", " "), x[2])}) %>% paste(collapse = "\n") %>% cat

# %% [markdown]
# #### Cost

# %% [markdown]
# Column Description

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='visit_cost'
          ")
data_info_ncol
data_info_ncol$column_name %>% paste(collapse = ",\n") %>% cat

# %% [markdown]
# Head

# %%
visit_occurrence_data <- grab_data("
SELECT 
    CAST(visit_cost_id AS STRING) AS visit_cost_id,
    CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id,
    CAST(currency_concept_id AS STRING) AS currency_concept_id,
    CAST(paid_copay AS STRING) AS paid_copay,
    CAST(paid_coinsurance AS STRING) AS paid_coinsurance,
    CAST(paid_toward_deductible AS STRING) AS paid_toward_deductible,
    CAST(paid_by_payer AS STRING) AS paid_by_payer,
    CAST(paid_by_coordination_benefits AS STRING) AS paid_by_coordination_benefits,
    CAST(total_out_of_pocket AS STRING) AS total_out_of_pocket,
    CAST(total_paid AS STRING) AS total_paid,
    CAST(payer_plan_period_id AS STRING) AS payer_plan_period_id
FROM
    `visit_cost`
LIMIT 50;
")
visit_occurrence_data %>% head

# %% [markdown]
# #### Cost Ext

# %% [markdown]
# Column Description

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='visit_cost_ext'
          ")
data_info_ncol

# %% [markdown]
# Head

# %%
visit_occurrence_data <- grab_data("
SELECT 
    CAST(visit_cost_id AS STRING) AS visit_cost_id,
    CAST(src_id AS STRING) AS src_id
FROM
    `visit_cost_ext`
LIMIT 50;
")
visit_occurrence_data %>% head

# %% [markdown]
# #### Detail

# %% [markdown]
# Column Description

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='visit_detail'
          ")
data_info_ncol
data_info_ncol$column_name %>% paste(collapse = ",\n") %>% cat

# %% [markdown]
# Head

# %%
#CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id_str,
visit_occurrence_data <- grab_data("
SELECT 
    CAST(visit_detail_id AS STRING) AS visit_detail_id,
    person_id,
    visit_detail_concept_id,
    visit_detail_start_date,
    visit_detail_start_datetime,
    visit_detail_end_date,
    visit_detail_end_datetime,
    visit_detail_type_concept_id,
    CAST(provider_id AS STRING) AS provider_id,
    CAST(care_site_id AS STRING) AS care_site_id,
    CAST(visit_detail_source_value AS STRING) AS visit_detail_source_value,
    CAST(visit_detail_source_concept_id AS STRING) AS visit_detail_source_concept_id,
    CAST(admitting_source_value AS STRING) AS admitting_source_value,
    CAST(admitting_source_concept_id AS STRING) AS admitting_source_concept_id,
    CAST(discharge_to_source_value AS STRING) AS discharge_to_source_value,
    CAST(discharge_to_concept_id AS STRING) AS discharge_to_concept_id,
    CAST(preceding_visit_detail_id AS STRING) AS preceding_visit_detail_id,
    CAST(visit_detail_parent_id AS STRING) AS visit_detail_parent_id,
    CAST(visit_occurrence_id AS STRING) AS visit_occurrence_id
FROM
    `visit_detail`
LIMIT 50;
")
visit_occurrence_data %>% head

# %%
col_name <- "visit_detail_concept_id"
col_num <- 3

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

visit_detail_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_detail.visit_detail_id) AS count_records
FROM
    `visit_detail`
LEFT JOIN
    `concept` standard_concept 
    ON visit_detail.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "visit_detail_type_concept_id"
col_num <- 8

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

visit_detail_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_detail.visit_detail_id) AS count_records
FROM
    `visit_detail`
LEFT JOIN
    `concept` standard_concept 
    ON visit_detail.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "visit_detail_source_concept_id"
col_num <- 12

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

visit_detail_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_detail.visit_detail_id) AS count_records
FROM
    `visit_detail`
LEFT JOIN
    `concept` standard_concept 
    ON visit_detail.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "admitting_source_concept_id"
col_num <- 14

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

visit_detail_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_detail.visit_detail_id) AS count_records
FROM
    `visit_detail`
LEFT JOIN
    `concept` standard_concept 
    ON visit_detail.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "discharge_to_concept_id"
col_num <- 16

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

visit_detail_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_detail.visit_detail_id) AS count_records
FROM
    `visit_detail`
LEFT JOIN
    `concept` standard_concept 
    ON visit_detail.{col_name} = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "visit_occurrence_id"
col_num <- 19

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat

visit_detail_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_summ[[1]] %>% comma %>% cat


# %% [markdown]
# #### Detail Ext

# %% [markdown]
# Column Description

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='visit_detail_ext'
          ")
data_info_ncol

# %% [markdown]
# Head

# %%
visit_occurrence_data <- grab_data("
SELECT 
    *
FROMHead
    `visit_detail_ext`
LIMIT 50;
")
visit_occurrence_data %>% head

# %%
col_name <- "visit_detail_id"
col_num <- 1

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_ext_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail_ext`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_ext_summ[[1]] %>% comma %>% cat

visit_detail_ext_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail_ext`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_ext_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT(CAST({col_name} AS STRING)) as {col_name}, 
    COUNT(DISTINCT {col_name}) AS count_records
FROM
    `visit_detail_ext`
GROUP BY {col_name}
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% head %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "src_id"
col_num <- 2

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_detail_ext_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_detail_ext`;
"))
"\n\tNumber of unique values: " %>% cat
visit_detail_ext_summ[[1]] %>% comma %>% cat

visit_detail_ext_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_detail_ext`;
"))
"\n\tNumber of blank values: " %>% cat
visit_detail_ext_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT {col_name} as {col_name}, 
    COUNT(DISTINCT {col_name}) AS count_records
FROM
    `visit_detail_ext`
GROUP BY {col_name}
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %% [markdown]
# #### Occurrence Ext

# %% [markdown]
# Column Description

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='visit_occurrence_ext'
          ")
data_info_ncol

# %% [markdown]
# Head

# %%
visit_occurrence_data <- grab_data("
SELECT 
    *
FROM
    `visit_occurrence_ext`
LIMIT 50;
")
visit_occurrence_data %>% head

# %%
col_name <- "visit_occurrence_id"
col_num <- 1

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_ext_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence_ext`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_ext_summ[[1]] %>% comma %>% cat

visit_occurrence_ext_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence_ext`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_ext_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT(CAST({col_name} AS STRING)) as {col_name}, 
    COUNT(DISTINCT {col_name}) AS count_records
FROM
    `visit_occurrence_ext`
GROUP BY {col_name}
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% head %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
col_name <- "src_id"
col_num <- 2

"col {col_num} | {col_name}" %>% str_glue %>% cat
visit_occurrence_ext_summ <- grab_data(str_glue("
SELECT
  COUNT(DISTINCT CAST({col_name} AS STRING)) AS num_unique,
FROM `visit_occurrence_ext`;
"))
"\n\tNumber of unique values: " %>% cat
visit_occurrence_ext_summ[[1]] %>% comma %>% cat

visit_occurrence_ext_summ <- grab_data(str_glue("
SELECT
  SUM(CASE WHEN {col_name} IS NULL THEN 1
           WHEN CAST({col_name} AS STRING) = '' THEN 1
           ELSE 0 END) AS blank_values,
FROM `visit_occurrence_ext`;
"))
"\n\tNumber of blank values: " %>% cat
visit_occurrence_ext_summ[[1]] %>% comma %>% cat

data <- grab_data(str_glue("                                              
SELECT 
    DISTINCT(CAST({col_name} AS STRING)) as {col_name}, 
    COUNT(DISTINCT {col_name}) AS count_records
FROM
    `visit_occurrence_ext`
GROUP BY {col_name}
ORDER BY count_records DESC
"))
"\n\nunique value counts:\n" %>% cat
data %>% mutate(count_records = comma(count_records)) %>% head %>% print(n = Inf) %>% capture.output %>% cat(sep = "\n")

# %%
