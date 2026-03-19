# %%
library(bigrquery)
library(questionr)
library(tidyverse)
library(reshape2)
library(scales)
library(tableone)
library(tidyr)
library(dplyr)
library(stringr)
library(RCurl)
library(RJSONIO)
library(lubridate)
library(svMisc)
library(IRdisplay)
library(plotly)
library(RColorBrewer)
library(grDevices)
source("easy_delete_copy_R.R")

IRdisplay::display_html('
<style>
.container { width:100% !important; } 
</style>

<style>
.output_png {
    display: table-cell;
    text-align: center;
    vertical-align: middle;
}
</style>
')
options(repr.plot.width=15, repr.plot.height=15)

# %%
my_bucket <- Sys.getenv("WORKSPACE_BUCKET")
my_bucket

# %%
data_info <- grab_data("
SELECT *
FROM INFORMATION_SCHEMA.COLUMNS
")

# %%
data_info %>% head

# %%
data_info_ncol <- grab_data("SELECT table_name, COUNT(DISTINCT column_name) count_cols
          FROM INFORMATION_SCHEMA.COLUMNS
          GROUP BY table_name
          ORDER BY table_name")
apply(data_info_ncol, 1, function(x){paste(str_pad(x[1], 28, "right", " "), x[2])}) %>% paste(collapse = "\n") %>% cat

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='concept'
          ")
data_info_ncol

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='observation'
          ")
data_info_ncol

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='person_ext'
          ")
data_info_ncol

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='visit_occurrence'
          ")
data_info_ncol

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='care_site'
          ")
data_info_ncol

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='location'
          ")
data_info_ncol

# %%
data_info_ncol <- grab_data("SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name='location_ext'
          ")
data_info_ncol

# %%
data <- grab_data("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT visit_occurrence.person_id) AS count_records
FROM
    `visit_occurrence`
LEFT JOIN
    `concept` standard_concept 
    ON visit_occurrence.visit_type_concept_id = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records
")
data %>% head

# %%
data_char_count <- data %>% mutate(char_count = nchar(standard_concept_name)) %>% mutate(char_count_num = nchar(as.character(count_records)))
max(data_char_count$char_count_num, na.rm = T)
data_short <- data %>%    
    mutate(short_name = str_sub(standard_concept_name,1, 147)) %>% 
    mutate(short_name_pad = str_pad(short_name, 147, side="right", pad="-")) %>%
    mutate(count_records_pad = str_pad(count_records, 6, side="left", pad="-"))
#apply(data_short[nrow(data_short):1,], 1, function(x){paste0(x["short_name_pad"], x["count_records_pad"])}) %>% paste(collapse = "\n") %>% cat

# %%
data <- grab_data("
SELECT 
    DISTINCT visit_occurrence.provider_id 
FROM
    `visit_occurrence`
")
data %>% head
data <- grab_data("
SELECT 
    DISTINCT visit_occurrence.care_site_id 
FROM
    `visit_occurrence`
")
data %>% head

# %%
data <- grab_data("
SELECT 
    DISTINCT care_site.care_site_id
FROM
    `care_site`
")
data %>% head

# %%
data <- grab_data("
SELECT 
    DISTINCT standard_concept.concept_name as standard_concept_name, 
    COUNT(DISTINCT observation.person_id) AS count_records
FROM
    `observation`
LEFT JOIN
    `concept` standard_concept 
    ON observation.observation_concept_id = standard_concept.concept_id
GROUP BY standard_concept_name
ORDER BY count_records
")
data %>% head

# %%
data_char_count <- data %>% mutate(char_count = nchar(standard_concept_name)) %>% mutate(char_count_num = nchar(as.character(count_records)))
#max(data_char_count$char_count, na.rm = T) %>% cat
data_short <- data %>%    
    mutate(short_name = str_sub(standard_concept_name,1, 255)) %>% 
    mutate(short_name_pad = str_pad(short_name, 255, side="right", pad="-")) %>%
    mutate(count_records_pad = str_pad(count_records, 6, side="left", pad="-"))
#apply(data_short[nrow(data_short):1,], 1, function(x){paste0(x["short_name_pad"], x["count_records_pad"])}) %>% paste(collapse = "\n") %>% cat

# %%
data_info <- grab_data("
SELECT *
FROM INFORMATION_SCHEMA.COLUMNS
WHERE table_name='observation'
")

# %%
# how many modules in survey data and how many questions in each module?
grab_data("SELECT survey, COUNT(DISTINCT question_concept_id) count_question
          FROM `ds_survey` 
          GROUP BY 1
          ORDER BY count_question")

# %%
observation_data <- grab_data("
SELECT 
    *
FROM
    `observation`
WHERE 
    observation_source_concept_id = 1585250
")
observation_data

# %%

# %%
zip_data <- grab_data("
SELECT 
    *
FROM
    `zip3_ses_map`
")


# %%
zip_data %>% head

# %%
str_sub(observation_data$value_as_string, 1, 1) %>% table

# %%
zip_data[zip_data$zip3 == 969,]

# %%
# Location of the datasets
(CURRENT_DATASET <- Sys.getenv('WORKSPACE_CDR'))

# %%
# Function to download data. Argument is a sql query.
download_data <- function(query) {
    tb <- bq_project_query(Sys.getenv('GOOGLE_PROJECT'), query)
    bq_table_download(tb)
}

# %%
#Obtain a all Question IDs from a few surveys
sql <- str_glue("SELECT DISTINCT person_id
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('Personal and Family Health History'
                                                )")
#WHERE survey IN ('Overall Health', 'The Basics', 'Lifestyle')")

these.ids <- download_data(sql)
head(these.ids)
dim(these.ids)
write.table(these.ids$person_id, "PFHH_ids.csv", row.names = F, col.names = F)

# %%
sql <- str_glue("SELECT DISTINCT person_id
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('Overall Health'
                                                )")

these.ids <- download_data(sql)
head(these.ids)
dim(these.ids)
write.table(these.ids$person_id, "OverallHealth_ids.csv", row.names = F, col.names = F)

# %%
sql <- str_glue("SELECT DISTINCT person_id
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('The Basics'
                                                )")

these.ids <- download_data(sql)
head(these.ids)
dim(these.ids)
write.table(these.ids$person_id, "TheBasics_ids.csv", row.names = F, col.names = F)

# %%
sql <- str_glue("SELECT DISTINCT person_id
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('Lifestyle'
                                                )")

these.ids <- download_data(sql)
head(these.ids)
dim(these.ids)
write.table(these.ids$person_id, "Lifestyle_ids.csv", row.names = F, col.names = F)

# %%
sql <- str_glue("SELECT DISTINCT person_id
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('Healthcare Access & Utilization'
                                                )")

these.ids <- download_data(sql)
head(these.ids)
dim(these.ids)
write.table(these.ids$person_id, "HAU_ids.csv", row.names = F, col.names = F)

# %%
