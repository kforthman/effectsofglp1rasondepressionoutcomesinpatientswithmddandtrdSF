library(DBI)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(arrow)
library(dplyr)

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
drug_class  <- read.csv(config$files$drug_class)
med_recode  <- read_csv(config$files$medication_recode, 
                        col_types = cols(
                          table          = col_factor(),
                          raw_name       = col_factor(),
                          canonical_name = col_factor(),
                          subclass       = col_factor()
                        ))
treatments_subclass_map <- med_recode %>%
  filter(table == "treatments", !is.na(subclass) & subclass != "") %>%
  distinct(canonical_name, subclass)

data_pull_date                  <- as.Date(config$data_pull_date)
data_sample_size                <- config$data_sample_size
target_drug                     <- config$target_drug
comparator_drugs                <- config$comparator_drugs
nontreatment_group              <- config$nontreatment_group
eligibility_inclusion_diagnoses <- config$eligibility_inclusion_diagnoses
null_values                     <- config$null_values
n_patient_partitions            <- config$n_patient_partitions
overwrite                       <- as.logical(config$overwrite)

all_drugs         <- c(target_drug, comparator_drugs)
comparator_groups <- c(nontreatment_group, comparator_drugs)
all_groups        <- c(target_drug, comparator_groups)

atc_drugs <- read_csv(config$files$atc_drugs, 
                      col_types = cols(
                        ATC_code         = col_factor(),
                        Name             = col_character(),
                        Category_Level_4 = col_factor(),
                        Category_Level_3 = col_factor(),
                        Category_Level_2 = col_factor(),
                        Category_Level_1 = col_factor()
                      )) %>%
  mutate(length = nchar(Name)) %>%
  arrange(Name) %>%
  mutate(Name = as.factor(Name))
cpt_acuity  <- read.csv(config$files$cpt_acuity)

if(!dir.exists("OutputData")){
  dir.create("OutputData")
}
if(!dir.exists("Parquet")){
  dir.create("Parquet")
}
if(!dir.exists("Parquet_batched")){
  dir.create("Parquet_batched")
}
if(!dir.exists("Parquet_batched_prepped")){
  dir.create("Parquet_batched_prepped")
}


# -- Validate column schema ----------------------------------------------------
check_schema_table(col_schema, "med_index_table",   config, conn = conProjects)
check_schema_table(col_schema, "diag_table",        config, conn = conProjects)
check_schema_table(col_schema, "mdd_data",          config, conn = conProjects)
check_schema_table(col_schema, "dte_cohort_data",   config, conn = conProjects)
check_schema_table(col_schema, "nonswitch_periods", config, conn = conProjects)
check_schema_table(col_schema, "psych_proc",        config, conn = conProjects)
check_schema_table(col_schema, "encounter_table",   config, conn = conProjects)
check_schema_table(col_schema, "med_table_ad",      config, conn = conProjects)
check_schema_table(col_schema, "med_table_ap",      config, conn = conProjects)
check_schema_table(col_schema, "med_table_hctz",   config, conn = conProjects)
check_schema_table(col_schema, "med_table_treat",   config, conn = conProjects)

# -- Write files as parquet ----------------------------------------------------

dbo_files <- config$files[grep("dbo.", config$files)]
for(i in 1:length(dbo_files)){
  this_name <- names(dbo_files[i])
  this_file <- dbo_files[[i]]
  
  if(file.exists(str_glue("Parquet/{this_name}")) & !overwrite){
    message(str_glue("Skipping writing {this_name} as parquet; file already exists. To overwrite, set variable overwrite to TRUE in the config file."))
    next
  }
  
  message(str_glue("Writing {this_name} as parquet."))
  
  DBI::dbGetQuery(conProjects, str_glue("SELECT * FROM {this_file}")) |>
    apply_col_types(col_schema, this_name) |>
    write_dataset(str_glue("Parquet/{this_name}"), format = "parquet")
}

csv_files <- config$files[grep(".csv", config$files)]
for(i in 1:length(csv_files)){
  this_name <- names(csv_files[i])
  this_file <- csv_files[[i]]
  
  if(!this_name %in% col_schema$table){next}
  
  if(file.exists(str_glue("Parquet/{this_name}")) & !overwrite){
    message(str_glue("Skipping writing {this_name} as parquet; file already exists. To overwrite, set variable overwrite to TRUE in the config file."))
    next
  }
  
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
    null_values         = null_values,
    strings_can_be_null = TRUE,
    true_values         = c("1", "true",  "True",  "TRUE"),
    false_values        = c("0", "false", "False", "FALSE")
  ) |>
    write_dataset(str_glue("Parquet/{this_name}"), format = "parquet")
}

# -- Check medication names --------------------------------------------------
all_ad_canonical <- med_recode %>% 
  filter(table == "antidepressant" & !canonical_name == "remove") %>% 
  pull(canonical_name)
missing_ad_class <- sort(setdiff(all_ad_canonical, drug_class$Name))
if(length(missing_ad_class) > 0){
  out_filename <- str_glue("OutputData/missing_ad_class.csv")
  warning(sprintf("[AD Drug Classes]: %d name(s) have no class entry, missing name(s) will be written to \"%s\":\n  %s",
                  length(missing_ad_class),
                  out_filename,
                  paste(missing_ad_class, collapse = "\n  ")))
  missing_recodes <- data.frame(raw_name = missing_ad_class, stringsAsFactors = FALSE)
  write.csv(missing_recodes, out_filename, row.names = FALSE)
}

all_canonical <- med_recode %>% 
  filter(table %in% c("antidepressant", "antipsychotic", "hydrochlorothiazide") & 
           !canonical_name == "remove") %>% 
  pull(canonical_name)
missing_atc <- sort(setdiff(all_canonical, atc_drugs$Name))
if(length(missing_atc) > 0){
  out_filename <- str_glue("OutputData/missing_atc_code.csv")
  warning(sprintf("[ATC Codes]: %d name(s) have no ATC code entry, missing name(s) will be written to \"%s\":\n  %s",
                  length(missing_atc),
                  out_filename,
                  paste(missing_atc, collapse = "\n  ")))
  missing_recodes <- data.frame(raw_name = missing_atc, stringsAsFactors = FALSE)
  write.csv(missing_recodes, out_filename, row.names = FALSE)
}


antidepressant_names <- open_dataset("Parquet/med_table_ad") %>% 
  filter(ExposureLabel %in% c("Antidepressant", "Misc. Psychotherapeutic")) %>%
  distinct(SimpleGenericName) %>%
  collect() %>%
  pull(SimpleGenericName)

check_recode(antidepressant_names,
             med_recode, 
             "antidepressant")

antipsychotic_names <- open_dataset("Parquet/med_table_ap") %>% 
  filter(ExposureLabel %in% c("Antipsychotic", "Misc. Psychotherapeutic")) %>%
  distinct(SimpleGenericName) %>%
  collect() %>%
  pull(SimpleGenericName)

check_recode(antipsychotic_names,
             med_recode, 
             "antipsychotics")

hydrochlorothiazide_names <- open_dataset("Parquet/med_table_hctz") %>% 
  filter(ExposureLabel %in% c("Hydrochlorothiazide")) %>%
  distinct(SimpleGenericName) %>%
  collect() %>%
  pull(SimpleGenericName)

check_recode(hydrochlorothiazide_names,
             med_recode, 
             "hydrochlorothiazide")

treatment_names <- open_dataset("Parquet/med_table_treat") %>% 
  filter(ExposureLabel %in% all_drugs) %>%
  distinct(SimpleGenericName) %>%
  collect() %>%
  pull(SimpleGenericName)

check_recode(treatment_names,
             med_recode, 
             "treatments")



# -- Batching ---------------------------------------------------------------

set.seed(123)

if(file.exists(str_glue("Parquet_batched/batch_assignment")) & !overwrite){
  message(str_glue("Skipping writing batch_assignment as parquet; file already exists. To overwrite, set variable overwrite to TRUE in the config file."))
  batch_assignment <- open_dataset("Parquet_batched/batch_assignment") %>%
    collect()
}else{
  batch_assignment <- open_dataset("Parquet/mdd_data")  %>%
    distinct(PatientDurableKey) %>%
    collect() %>%
    slice_sample(prop = data_sample_size) %>%
    mutate(batch_number = ((row_number() - 1) %% n_patient_partitions) + 1)
  
  write_dataset(
    batch_assignment,
    path = "Parquet_batched/batch_assignment",
    format = "parquet",
    partitioning = "batch_number"
  )
}
batch_assignment <- arrow_table(batch_assignment)

all_tables <- list.files("Parquet")
for(this_table in all_tables){
  if(file.exists(str_glue("Parquet_batched/{this_table}")) & !overwrite){
    message(str_glue("Skipping writing {this_table} as batched parquet; file already exists. To overwrite, set variable overwrite to TRUE in the config file."))
    next
  }
  
  message(str_glue("Writing {this_table} as batched parquet."))
  
  open_dataset(str_glue("Parquet/{this_table}")) %>%
    inner_join(batch_assignment, by = "PatientDurableKey") %>%
    write_dataset(
      path = str_glue("Parquet_batched/{this_table}"),
      format = "parquet",
      partitioning = "batch_number"
    )
}

# --  ---------------------------------------------------------------

for(batch_num in 1:n_patient_partitions){
  
  message("Processing batch ", batch_num, "...")
  
  # medication index table
  med_index_table <- open_dataset("Parquet_batched/med_index_table") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    mutate(batch_number = batch_num)
  
  if(nrow(med_index_table) == 0){
    warning(str_glue("Table med_index_table is empty for batch {batch_num}"))
    }
  
  write_dataset(
    med_index_table,
    path = "Parquet_batched_prepped/med_index_table",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  # diagnosis table
  diag_table <- open_dataset("Parquet_batched/diag_table") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    arrange(PatientDurableKey) %>%
    dplyr::select(c("PatientDurableKey",
                    setdiff(col_schema %>%
                              filter(table == "diag_table") %>%
                              pull(column),
                            col_schema %>%
                              filter(table == "mdd_data") %>%
                              pull(column)
                    ))) %>%
    mutate(batch_number = batch_num)
  
  if(nrow(diag_table) == 0){
    warning(str_glue("Table diag_table is empty for batch {batch_num}"))
  }
  
  write_dataset(
    diag_table,
    path = "Parquet_batched_prepped/diag_table",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  # Patients with MDD, no Bipolar Disorder, no Schizophrenia
  mdd_data <- open_dataset("Parquet_batched/mdd_data") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    rename(Sex = "PatientSex") %>%
    mutate(Race = case_when(
      !is.na(SecondRace) | !is.na(ThirdRace) | !is.na(FourthRace) | !is.na(FifthRace) | MultiRacial ~ "Multi-Race",
      FirstRace == "American Indian or Alaska Native" ~ "American Indian or Alaska Native",
      FirstRace == "Asian" ~ "Asian",
      FirstRace == "Black or African American" ~ "Black or African American",
      FirstRace == "Native Hawaiian or Other Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
      FirstRace == "White" ~ "White or Caucasian"
    ),
    Race_Ethnicity = case_when(
      !is.na(Ethnicity) & Ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
      TRUE ~ Race
    ),
    Race_Ethnicity_white = Race_Ethnicity == "White or Caucasian",
    Sex_male = Sex == "Male",
    Age = time_length(interval(BirthDate, data_pull_date), "years")
    ) %>%
    filter(Sex %in% c("Male", "Female") & 
             !is.na(Sex) & 
             !is.na(Race_Ethnicity) & 
             meets_diagnosis_eligibility_criteria
    ) %>%
    left_join(diag_table,
              by = "PatientDurableKey") %>%
    mutate(across(ends_with("_FirstDiagnosis"),
                  ~ if_else(is.na(.), FALSE, TRUE),
                  .names = "{sub('_FirstDiagnosis$', '', .col)}")) %>%
    left_join(med_index_table,
              by = "PatientDurableKey") %>%
    dplyr::select(
      -FirstRace,
      -SecondRace,
      -ThirdRace,
      -FourthRace,
      -FifthRace,
      -MultiRacial,
      -Ethnicity
    ) %>%
    mutate(across(ends_with("_Index"),
                  ~ if_else(is.na(.), FALSE, TRUE),
                  .names = "{sub('_Index$', '_Use', .col)}")) %>%
    mutate(batch_number = batch_num)
  
  if(nrow(mdd_data) == 0){
    warning(str_glue("Table mdd_data is empty for batch {batch_num}"))
  }
  
  write_dataset(
    mdd_data,
    path = "Parquet_batched_prepped/mdd_data",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(diag_table)
  rm(med_index_table)
  gc()
  
  # DTE cohort data
  dte_cohort_data <- open_dataset("Parquet_batched/dte_cohort_data") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    left_join(mdd_data,
              by = "PatientDurableKey") %>%
    filter(meets_diagnosis_eligibility_criteria)
  
  for(i in 1:length(all_drugs)){
    this_drug <-  all_drugs[i]
    
    var_name_index <- paste0(this_drug, "_Index")
    var_name_age_at_index_years <- paste0(this_drug, "_age_at_index_years")
    var_name_mdd_to_index_days <- paste0(this_drug, "_mdd_to_index_days")
    
    dte_cohort_data <- dte_cohort_data %>% 
      mutate(
        !!sym(var_name_mdd_to_index_days) := time_length(interval(MDD_Index, !!sym(var_name_index)), "days"),
        !!sym(var_name_age_at_index_years) := time_length(interval(BirthDate, !!sym(var_name_index)), "years")
      )
  }
  
  dte_cohort_data <- dte_cohort_data %>% 
    dplyr::select(PatientDurableKey, 
                  meets_diagnosis_eligibility_criteria, 
                  MDD_Index, 
                  BirthDate, 
                  sort(setdiff(names(.), c("PatientDurableKey", "meets_diagnosis_eligibility_criteria", "MDD_Index", "BirthDate")))
    ) %>%
    mutate(batch_number = batch_num)
  
  if(nrow(dte_cohort_data) == 0){
    warning(str_glue("Table dte_cohort_data is empty for batch {batch_num}"))
  }
  
  write_dataset(
    dte_cohort_data,
    path = "Parquet_batched_prepped/dte_cohort_data",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(dte_cohort_data)
  gc()
  
  # Nonswitch periods
  nonswitch_periods <- open_dataset("Parquet_batched/nonswitch_periods") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, meets_diagnosis_eligibility_criteria),
              by = "PatientDurableKey") %>%
    filter(meets_diagnosis_eligibility_criteria) %>%
    mutate(
      at_6_months_after_start_date = StartDate + days(180),
      at_12_months_before_end_date = EndDate - days(365),
      tfe_at_index_bgn = floor(time_length(interval(MDD_Index, at_6_months_after_start_date), "days")),
      tfe_at_index_end = floor(time_length(interval(MDD_Index, at_12_months_before_end_date), "days")),
    ) %>%
    mutate(batch_number = batch_num)
  
  if(nrow(nonswitch_periods) == 0){
    warning(str_glue("Table nonswitch_periods is empty for batch {batch_num}"))
  }
  
  write_dataset(
    nonswitch_periods,
    path = "Parquet_batched_prepped/nonswitch_periods",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(nonswitch_periods)
  rm(mdd_data)
  gc()
  
  # Psych procedures table
  psych_proc <- open_dataset("Parquet_batched/psych_proc") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    left_join(cpt_acuity %>% 
                dplyr::select(source_concept_code, level), 
              by = join_by("CPTCode" == "source_concept_code")) %>%
    mutate(batch_number = batch_num)
  
  if(nrow(psych_proc) == 0){
    warning(str_glue("Table psych_proc is empty for batch {batch_num}"))
  }
  
  write_dataset(
    psych_proc,
    path = "Parquet_batched_prepped/psych_proc",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(psych_proc)
  gc()
  
  # Encounters table
  encounter_table <- open_dataset("Parquet_batched/encounter_table") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    mutate(StartVisit = as.Date(StartVisit),
           EndVisit   = as.Date(EndVisit)) %>%
    mutate(EndVisit = pmax(EndVisit, StartVisit)) %>% # End visit should not come before start visit.
    mutate(batch_number = batch_num) 
  
  if(nrow(encounter_table) == 0){
    warning(str_glue("Table encounter_table is empty for batch {batch_num}"))
  }
  
  write_dataset(
    encounter_table,
    path = "Parquet_batched_prepped/encounter_table",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(encounter_table)
  gc()
  
  # Antidepressants
  med_table_ad <- open_dataset("Parquet_batched/med_table_ad") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    filter(ExposureLabel %in% c("Antidepressant", "Misc. Psychotherapeutic")) %>%
    apply_recode(med_recode, "antidepressant") %>%
    left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
    filter(substr(ATC_code, 1, 4) == "N06A") %>%
    mutate(batch_number = batch_num)
  
  if(nrow(med_table_ad) == 0){
    warning(str_glue("Table med_table_ad is empty for batch {batch_num}"))
  }
  
  write_dataset(
    med_table_ad,
    path = "Parquet_batched_prepped/med_table_ad",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(med_table_ad)
  gc()
  
  # Antipsychotics
  med_table_ap <- open_dataset("Parquet_batched/med_table_ap") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    filter(ExposureLabel %in% c("Antipsychotic", "Misc. Psychotherapeutic")) %>%
    apply_recode(med_recode, "antipsychotics") %>%
    left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
    filter(substr(ATC_code, 1, 5) %in% c("N05AE", "N05AH", "N05AL", "N05AN", "N05AX") & ATC_code != "N05AH02") %>%
    mutate(batch_number = batch_num)
  
  if(nrow(med_table_ap) == 0){
    warning(str_glue("Table med_table_ap is empty for batch {batch_num}"))
  }
  
  write_dataset(
    med_table_ap,
    path = "Parquet_batched_prepped/med_table_ap",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(med_table_ap)
  gc()
  
  # Hydrochlorothiazide
  med_table_hctz <- open_dataset("Parquet_batched/med_table_hctz") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    filter(ExposureLabel %in% c("Hydrochlorothiazide")) %>%
    apply_recode(med_recode, "hydrochlorothiazide") %>%
    left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
    filter(ATC_code == "C03AA03") %>%
    mutate(batch_number = batch_num)
  
  if(nrow(med_table_hctz) == 0){
    warning(str_glue("Table med_table_hctz is empty for batch {batch_num}"))
  }
  
  write_dataset(
    med_table_hctz,
    path = "Parquet_batched_prepped/med_table_hctz",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  rm(med_table_hctz)
  gc()
  
  # Treatments
  med_table_treat <- open_dataset("Parquet_batched/med_table_treat") %>%
    filter(batch_number == batch_num) %>%
    collect() %>%
    filter(ExposureLabel %in% all_drugs) %>%
    apply_recode(med_recode, "treatments") %>%
    separate_rows(SimpleGenericName, sep = "/") %>%
    left_join(treatments_subclass_map, by = c("SimpleGenericName" = "canonical_name")) %>%
    rename(PharmaceuticalSubclass = "subclass") %>%
    dplyr::select(-ExposureLabel) %>%
    mutate(batch_number = batch_num)
  
  if(nrow(med_table_treat) == 0){
    warning(str_glue("Table med_table_treat is empty for batch {batch_num}"))
  }
  
  write_dataset(
    med_table_treat,
    path = "Parquet_batched_prepped/med_table_treat",
    format = "parquet",
    partitioning = "batch_number"
  )
  
  for(this_drug in all_drugs){
    this_table_name <- paste0("med_table_treat_", this_drug)
    this_med_table_treat <- med_table_treat %>%
      filter(PharmaceuticalSubclass == this_drug)
    
    if(nrow(this_med_table_treat) == 0){
      warning(str_glue("Table {this_table_name} is empty for batch {batch_num}"))
    }
    
    write_dataset(
      this_med_table_treat,
      path = str_glue("Parquet_batched_prepped/{this_table_name}"),
      format = "parquet",
      partitioning = "batch_number"
    )
    
    rm(this_med_table_treat)
    gc()
  }
  
  rm(med_table_treat)
  gc()
}

# Misc
rm(col_schema)
rm(treatments_subclass_map)
gc()

if (!is.null(conProjects)) {
  DBI::dbDisconnect(conProjects)
  rm(conProjects)
  gc()
}