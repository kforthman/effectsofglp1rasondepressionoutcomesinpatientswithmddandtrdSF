# %%
# Load shared workbench utility functions and install/load required packages.
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
library(tidyverse)
library(bigrquery)

# This query represents dataset "get_BMI" for domain "measurement" and was generated for All of Us Controlled Tier Dataset v8
# Retrieves all body weight (concept_id 3025315) and body height (concept_id 3036277) measurements
# for all participants, along with associated metadata (units, visit type, source codes, etc.).
# The cb_criteria subquery resolves the concept hierarchy to capture all descendant concepts
# under the two root concept IDs.
dataset_40015557_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (3025315, 3036277)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) measurement 
    LEFT JOIN
        `concept` m_standard_concept 
            ON measurement.measurement_concept_id = m_standard_concept.concept_id 
    LEFT JOIN
        `concept` m_type 
            ON measurement.measurement_type_concept_id = m_type.concept_id 
    LEFT JOIN
        `concept` m_operator 
            ON measurement.operator_concept_id = m_operator.concept_id 
    LEFT JOIN
        `concept` m_value 
            ON measurement.value_as_concept_id = m_value.concept_id 
    LEFT JOIN
        `concept` m_unit 
            ON measurement.unit_concept_id = m_unit.concept_id 
    LEFT JOIn
        `visit_occurrence` v 
            ON measurement.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` m_visit 
            ON v.visit_concept_id = m_visit.concept_id 
    LEFT JOIN
        `concept` m_source_concept 
            ON measurement.measurement_source_concept_id = m_source_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the measurement data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_40015557_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  #strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_40015557",
  "measurement_40015557_*.csv")
message(str_glue('The data will be written to {measurement_40015557_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_40015557_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_40015557_path,
  destination_format = "CSV")



# %%
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_40015557_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), measurement_type_concept_name = col_character(), operator_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), measurement_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), unit_source_value = col_character(), value_source_value = col_character())
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
dataset_40015557_measurement_df <- read_bq_export_from_workspace_bucket(measurement_40015557_path)

dim(dataset_40015557_measurement_df)

head(dataset_40015557_measurement_df, 5)

# %%
library(tidyverse)
library(bigrquery)

# This query represents dataset "get_BMI" for domain "person" and was generated for All of Us Controlled Tier Dataset v8
# Retrieves date of birth and sex at birth for all participants.
# Sex at birth is joined from the concept table to get a human-readable label.
dataset_40015557_person_sql <- paste("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the person data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_40015557_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  #strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_40015557",
  "person_40015557_*.csv")
message(str_glue('The data will be written to {person_40015557_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_40015557_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_40015557_path,
  destination_format = "CSV")



# %%
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_40015557_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(sex_at_birth = col_character())
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
dataset_40015557_person_df <- read_bq_export_from_workspace_bucket(person_40015557_path)

dim(dataset_40015557_person_df)

head(dataset_40015557_person_df, 5)

# %%
# Trim to relevant columns, parse datetime, remove rows with missing datetime or value,
# join with person table to get date of birth, compute age at measurement,
# and restrict to adults (age >= 18).
data <- dataset_40015557_measurement_df %>%
dplyr::select(person_id, standard_concept_name, measurement_datetime, measurement_type_concept_name, operator_concept_name, value_as_number, unit_concept_name) %>%
mutate(measurement_datetime = ymd_hms(measurement_datetime)) %>%
filter(!is.na(measurement_datetime)) %>%
filter(!is.na(value_as_number)) %>%
left_join(dataset_40015557_person_df, by = join_by(person_id)) %>%
mutate(age_at_measurement = interval(date_of_birth, measurement_datetime) / dyears()) %>%
filter(age_at_measurement >= 18) %>%
arrange(person_id, measurement_datetime) %>%
dplyr::select(person_id, standard_concept_name, measurement_datetime, measurement_type_concept_name, operator_concept_name, value_as_number, unit_concept_name, age_at_measurement)

data %>% head()

# %%
# Extract most recent height record per participant.
# Only one height record is kept (no ties) to use as a stable denominator for BMI.
recent_height <- data %>%
filter(standard_concept_name == "Body height") %>%
group_by(person_id) %>%
slice_max(measurement_datetime, n = 1, with_ties = FALSE)

recent_height %>% head

# %%
# Extract all weight records per participant.
# All records are retained so BMI can be computed at each weight measurement date.
all_weight <- data %>%
filter(standard_concept_name == "Body weight")

all_weight %>% head

# %%
# Inspect height units to confirm expected unit (centimeters) and detect any unexpected values.
recent_height$unit_concept_name %>% table

# %%
# Inspect weight units to confirm expected unit (kilograms) and detect any unexpected values.
all_weight$unit_concept_name %>% table

# %%
# Join each weight record to the participant's most recent height record,
# convert height from centimeters to meters, then compute BMI = weight(kg) / height(m)^2.
# Also join person table to retain sex at birth for downstream stratification.
bmi <- all_weight %>%
left_join(recent_height, by = join_by(person_id), suffix = c("_weight", "_height")) %>%
mutate(value_as_number_height = value_as_number_height*0.01, unit_concept_name_height = "meter") %>%
mutate(bmi = value_as_number_weight/(value_as_number_height^2)) %>%
left_join(dataset_40015557_person_df, by = join_by(person_id))

bmi %>% head

# %%
# Sanity-check histogram of all BMI records for men (BMI < 100 to exclude implausible values).
hist(bmi %>% filter(bmi < 100) %>% filter(sex_at_birth == "Male") %>% pull(bmi), main = "Histogram of BMI in Men >=18 yo", xlab = "BMI")

# %%
# Sanity-check histogram of all BMI records for women (BMI < 100 to exclude implausible values).
hist(bmi %>% filter(bmi < 100) %>% filter(sex_at_birth == "Female") %>% pull(bmi), main = "Histogram of BMI in Women >=18 yo", xlab = "BMI")

# %%
# Reduce to one row per participant: the weight record with the most recent measurement date.
# This `recent_bmi` table is used downstream to represent each participant's current BMI.
recent_bmi <- bmi %>%
group_by(person_id) %>%
slice_max(measurement_datetime_weight, n = 1, with_ties = FALSE) %>%
ungroup

recent_bmi %>% head

# %%
# Sanity-check histogram of most recent BMI for men.
hist(recent_bmi %>% filter(bmi < 100) %>% filter(sex_at_birth == "Male") %>% pull(bmi), main = "Histogram of most recent BMI in Men >=18 yo", xlab = "BMI")

# %%
# Sanity-check histogram of most recent BMI for women.
hist(recent_bmi %>% filter(bmi < 100) %>% filter(sex_at_birth == "Female") %>% pull(bmi), main = "Histogram of most recent BMI in Women >=18 yo", xlab = "BMI")

# %%
# Proportion of participants with BMI >= 25 (overweight or obese) in the most recent record.
sum(recent_bmi$bmi >= 25, na.rm = T) / nrow(recent_bmi)

# %%
# Quick manual check: sum of overweight (30.7%) and obese (42.4%) percentages from histogram inspection.
30.7+42.4

# %%
# Finalize types for the full bmi table before saving:
# person_id cast to character for consistent joining downstream;
# datetimes cast to Date; string columns cast to factor.
bmi <- bmi %>%
mutate(person_id = as.character(person_id)) %>%
mutate(across(c(measurement_datetime_weight, measurement_datetime_height, date_of_birth), ~as.Date(.))) %>%
mutate(across(c(standard_concept_name_weight, measurement_type_concept_name_weight, operator_concept_name_weight, unit_concept_name_weight,
               standard_concept_name_height, measurement_type_concept_name_height, operator_concept_name_height, unit_concept_name_height,
               sex_at_birth), ~as.factor(.)))

# %%
bmi %>% head

# %%
# Apply the same type finalization to recent_bmi.
recent_bmi <- recent_bmi %>%
mutate(person_id = as.character(person_id)) %>%
mutate(across(c(measurement_datetime_weight, measurement_datetime_height, date_of_birth), ~as.Date(.))) %>%
mutate(across(c(standard_concept_name_weight, measurement_type_concept_name_weight, operator_concept_name_weight, unit_concept_name_weight,
               standard_concept_name_height, measurement_type_concept_name_height, operator_concept_name_height, unit_concept_name_height,
               sex_at_birth), ~as.factor(.)))

# %%
recent_bmi %>% head

# %%
# Save outputs:
# - bmi.rds: all weight records with computed BMI (used if longitudinal BMI is needed)
# - recent_bmi.rds: one row per participant with their most recent BMI (used in get_DTE_cohort)
save(bmi, file = "bmi.rds")
save(recent_bmi, file = "recent_bmi.rds")

# %%
