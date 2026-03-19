# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %% [markdown]
# # Visit table info

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
LIMIT 10;
")
visit_occurrence_data
visit_occurrence_data %>% colnames %>% paste(collapse = ", \n\t") %>% cat

# %% [markdown]
# # Procedure table info

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
LIMIT 10;
")
procedure_occurrence_data
procedure_occurrence_data %>% colnames %>% paste(collapse = ", \n\t") %>% cat

# %% [markdown]
# # Combined procedure/visit table

# %%
po_visit_data <- grab_data("

WITH
-- 1) Blank (NULL) counts in each table
po_blank AS (
  SELECT COUNT(*) AS blank_rows
  FROM procedure_occurrence
  WHERE visit_occurrence_id IS NULL
),
vo_blank AS (
  SELECT COUNT(*) AS blank_rows
  FROM visit_occurrence
  WHERE visit_occurrence_id IS NULL
),

-- Total rows
po_rows AS (
  SELECT COUNT(*) AS total_rows
  FROM procedure_occurrence
),
vo_rows AS (
  SELECT COUNT(*) AS total_rows
  FROM visit_occurrence
),

-- Distinct non-null visit_occurrence_id sets
po_ids AS (
  SELECT DISTINCT visit_occurrence_id
  FROM procedure_occurrence
  WHERE visit_occurrence_id IS NOT NULL
),
vo_ids AS (
  SELECT DISTINCT visit_occurrence_id
  FROM visit_occurrence
  WHERE visit_occurrence_id IS NOT NULL
),

po_distinct_ids AS(
    SELECT COUNT(*) AS cnt
    FROM po_ids
),
vo_distinct_ids AS(
    SELECT COUNT(*) AS cnt
    FROM vo_ids
),

-- 2) Distinct IDs that map between tables (intersection)
mapped_ids AS (
  SELECT COUNT(*) AS cnt
  FROM po_ids p
  INNER JOIN vo_ids v
    ON p.visit_occurrence_id = v.visit_occurrence_id
),

-- 3) Distinct IDs that do NOT map (set differences)
po_only_ids AS (
  SELECT COUNT(*) AS cnt
  FROM po_ids p
  LEFT JOIN vo_ids v
    ON p.visit_occurrence_id = v.visit_occurrence_id
  WHERE v.visit_occurrence_id IS NULL
),
vo_only_ids AS (
  SELECT COUNT(*) AS cnt
  FROM vo_ids v
  LEFT JOIN po_ids p
    ON v.visit_occurrence_id = p.visit_occurrence_id
  WHERE p.visit_occurrence_id IS NULL
)

SELECT '01' AS row_number, 'procedure_occurrence: total rows' AS metric, (SELECT total_rows FROM po_rows) AS value
UNION ALL
SELECT '02', 'procedure_occurrence: total distinct visit_occurrence_id', (SELECT cnt FROM po_distinct_ids)
UNION ALL
SELECT '03', 'procedure_occurrence: blank visit_occurrence_id rows', (SELECT blank_rows FROM po_blank)
UNION ALL
SELECT '04', 'visit_occurrence: total rows', (SELECT total_rows FROM vo_rows)
UNION ALL
SELECT '05', 'visit_occurrence: total distinct visit_occurrence_id', (SELECT cnt FROM vo_distinct_ids)
UNION ALL
SELECT '06', 'visit_occurrence: blank visit_occurrence_id rows', (SELECT blank_rows FROM vo_blank)
UNION ALL
SELECT '07', 'distinct visit_occurrence_id present in both tables', (SELECT cnt FROM mapped_ids)
UNION ALL
SELECT '08', 'distinct visit_occurrence_id only in procedure_occurrence', (SELECT cnt FROM po_only_ids)
UNION ALL
SELECT '09', 'distinct visit_occurrence_id only in visit_occurrence', (SELECT cnt FROM vo_only_ids)
")

po_visit_data %>% as.data.frame %>% mutate(value = comma(value)) %>% arrange(row_number)

# %%
15822562 + 341546

# %%

# %%
visit_detail_data <- grab_data("
SELECT *
FROM visit_occurrence_ext
LIMIT 500")
visit_detail_data %>% head

# %%
visit_detail_data <- grab_data("
SELECT 
    DISTINCT CASE WHEN src_id LIKE '%EHR%' then 'EHR' ELSE src_id END as data_source,
    COUNT(DISTINCT visit_occurrence.visit_occurrence_id) AS count_records
FROM visit_occurrence
LEFT JOIN visit_occurrence_ext
    ON visit_occurrence.visit_occurrence_id = visit_occurrence_ext.visit_occurrence_id
GROUP BY data_source
ORDER BY count_records DESC")
visit_detail_data %>% dim
visit_detail_data
