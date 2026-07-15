library(DBI)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(arrow)
library(dplyr)
library(odbc)

source("helper_functions.R")

config <- fromJSON("config-FullSample.json")

conProjects <- dbConnect(
  odbc(),
  .connection_string = sprintf(
    "Driver={%s};Server=%s;Database=%s;Trusted_Connection=%s;",
    config$database$driver,
    config$database$server,
    config$database$database,
    config$database$trusted_connection
  ),
  timeout = config$database$timeout
)

col_schema  <- read.csv(config$files$column_schema,   stringsAsFactors = FALSE)
n_patient_partitions            <- config$n_patient_partitions

this_name <- "mdd_data"
this_file <- "dbo.PatientListSample"

DBI::dbGetQuery(conProjects, str_glue("SELECT * FROM {this_file}")) |>
  apply_col_types(col_schema, this_name) |>
  dplyr::select(PatientDurableKey,
                MaritalStatus,
                GenderIdentity,
                SviHouseholdCompositionPctlRankingByZip2018_X,
                SviHousingTypeTransportationPctlRankingByZip2018_X,
                SviMinorityStatusLanguagePctlRankingByZip2018_X,
                SviOverallPctlRankingByZip2018_X,
                SviSocioeconomicPctlRankingByZip2018_X) |>
  write_dataset(str_glue("Parquet/table1_add_demo"), format = "parquet")

batch_assignment <- open_dataset("Parquet_batched/batch_assignment") %>%
  collect()

open_dataset(str_glue("Parquet/table1_add_demo")) %>%
  inner_join(batch_assignment, by = "PatientDurableKey") %>%
  write_dataset(
    path = str_glue("Parquet_batched/table1_add_demo"),
    format = "parquet",
    partitioning = "batch_number"
  )

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  
  # medication index table
  table1_add_demo <- open_dataset("Parquet_batched/table1_add_demo") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    mutate(batch_number = batch_num) %>%
    mutate(
      GenderIdentity = case_when(
        GenderIdentity == "Transgender Female / Male-to-Female" ~ "Transgender",
        GenderIdentity == "Transgender Male / Female-to-Male"   ~ "Transgender",
        GenderIdentity == "Female"                              ~ "Female",
        GenderIdentity == "Male"                                ~ "Male",
        TRUE ~ NA
      ),
      MaritalStatus = case_when(
        MaritalStatus %in% c("Married", "Legally Separated", "Polygamous", "Interlocutory")  ~ "Married",
        MaritalStatus == "Divorced"                                                          ~ "Divorced",
        MaritalStatus == "Widowed"                                                           ~ "Widowed",
        MaritalStatus %in% c("Domestic partner", "Unmarried", "Never Married", "Common Law") ~ "Unmarried",
        MaritalStatus %in% c("Unknown")                                                      ~ NA,
        TRUE                                                                                 ~ NA
      )
    )
  
  if(nrow(table1_add_demo) == 0){
    warning(str_glue("Table table1_add_demo is empty for batch {batch_num}"))
  }
  
  write_dataset(
    table1_add_demo,
    path = "Parquet_batched_prepped/table1_add_demo",
    format = "parquet",
    partitioning = "batch_number"
  )
}
