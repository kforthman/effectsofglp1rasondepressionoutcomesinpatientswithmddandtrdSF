library(DBI)
library(arrow)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(lubridate)

source("helper_functions.R")

config <- fromJSON("config-FullSample.json")

if (!is.null(config$database)) {
  library(DBI)
  library(odbc)
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
} else {
  conProjects <- NULL
}

col_schema  <- read.csv(config$files$column_schema,   stringsAsFactors = FALSE)
null_values <- c("", "NA", "NULL", "null", "*Unspecified")

# Map a column_schema.csv "type" value to an Arrow data type.
schema_type_to_arrow <- function(type) {
  switch(type,
         character = utf8(),
         date      = date32(),
         logical   = boolean(),
         integer   = int32(),
         double    = float64(),
         factor    = utf8(),
         stop(sprintf("Unknown schema type: %s", type))
  )
}

# Build an Arrow Schema for one source table from the row-per-column schema
# data frame (Data/column_schema.csv). Returns NULL if the table is not in
# the schema (e.g. reference CSVs under Data/).
make_arrow_schema_csv <- function(col_schema, table_name) {
  specs <- col_schema[col_schema$table == table_name, ]
  if (nrow(specs) == 0) return(NULL)
  fields <- lapply(seq_len(nrow(specs)), function(i) {
    field(specs$column[i], schema_type_to_arrow(specs$type[i]))
  })
  do.call(arrow::schema, fields)
}

# dbo_files <- config$files[grep("dbo.", config$files)]
# for(i in 1:length(dbo_files)){
#   this_name <- names(dbo_files[i])
#   this_file <- dbo_files[[i]]
#   
#   message(str_glue("Writing {this_name} as parquet."))
#   
#   DBI::dbGetQuery(conProjects, str_glue("SELECT * FROM {this_file}")) |>
#     apply_col_types(col_schema, this_name) |>
#     write_dataset(str_glue("Parquet/{this_name}"), format = "parquet")
# }

csv_files <- config$files[grep(".csv", config$files)]
for(i in 1:length(csv_files)){
  this_name <- names(csv_files[i])
  this_file <- csv_files[[i]]
  
  if(!this_name %in% col_schema$table){next}
  
  message(str_glue("Writing {this_name} as parquet."))
  
  arrow_schema <- make_arrow_schema_csv(col_schema, this_name)
  file_cols <- names(read_csv(this_file, n_max = 0, show_col_types = FALSE))
  arrow_schema <- arrow_schema[match(file_cols, arrow_schema$names)]
  
  open_dataset(
    this_file,
    format = "csv",
    col_types = arrow_schema,
    col_names = arrow_schema$names,   # use schema's column names
    skip = 1,                          # skip the header row in the file
    na = null_values
  ) |>
    write_dataset(str_glue("Parquet/{this_name}"), format = "parquet")
}

# Then preview without re-hitting SQL Server
arrow::open_dataset("mdd_patient_list.parquet") |>
  head(20) |>
  collect()