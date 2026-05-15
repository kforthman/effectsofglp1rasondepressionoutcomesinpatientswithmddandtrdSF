# Version 1 - save as parquet with numerically encoded character columns
# combine AD and AP into one table.

library(arrow)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(lubridate)

source("helper_functions.R")

config <- fromJSON("config.json")


atc_drugs <- read_csv(config$files$atc_drugs, 
                      col_types = cols(
                        ATC_code         = col_character(),
                        Name             = col_character(),
                        Category_Level_4 = col_character(),
                        Category_Level_3 = col_character(),
                        Category_Level_2 = col_character(),
                        Category_Level_1 = col_character()
                      )) %>%
  mutate(length = nchar(Name)) %>%
  arrange(Name) %>%
  mutate(Name = as.factor(Name))

drug_class <- read_csv(config$files$drug_class,
                       col_types = cols(
                         Name = col_character(),
                         Type = col_character()
                       ))

med_recode  <- read_csv(config$files$medication_recode, 
                        col_types = cols(
                          table          = col_character(),
                          raw_name       = col_character(),
                          canonical_name = col_character(),
                          subclass       = col_character()
                        )) %>%
  left_join(atc_drugs, by = join_by(canonical_name == Name)) %>%
  left_join(drug_class, by = join_by(canonical_name == Name)) %>%
  rename(Category_Level_5 = "Type") %>%
  relocate(Category_Level_5, .before = Category_Level_4) |>
  mutate(Category_Level_5 = ifelse(Category_Level_3 == "ANTIPSYCHOTICS", 
                                   "Other Antipsychotics",
                                   Category_Level_5)) |>
  mutate(canonical_name_int = as.integer(as.factor(canonical_name)), 
         Category_Level_5_int = as.integer(as.factor(Category_Level_5)), 
         Category_Level_3_int = as.integer(as.factor(Category_Level_3)))

# Build a lookup tibble from your med_recode
lookup_df <- med_recode |>
  dplyr::filter(table == "antidepressant") |>
  dplyr::filter(Category_Level_3 == "ANTIDEPRESSANTS") |>
  dplyr::filter(canonical_name  != "remove") |>
  dplyr::select(SimpleGenericName = raw_name, canonical_name_int, 
                Category_Level_5_int, Category_Level_3_int)

open_dataset(
  "Z:/Project D91201F/PatientMedications_Antidepressants.csv",
  format = "csv",
  convert_options = CsvConvertOptions$create(
    null_values = c("", "NA", "NULL", "null", "*Unspecified"),
    strings_can_be_null = TRUE
  )
) |>
  filter(ExposureLabel %in% c("Antidepressant", "Misc. Psychotherapeutic")) |>
  dplyr::select(-ExposureLabel) |>
  left_join(lookup_df, by = "SimpleGenericName") |>
  dplyr::filter(!is.na(canonical_name_int)) |>
  dplyr::select(-SimpleGenericName) |>
  dplyr::rename(SimpleGenericName = canonical_name_int) |>
  mutate(DaysSupply = Quantity/DailyDose) |>
  dplyr::select(-Quantity, -DailyDose) |>
  write_dataset("OutputData/PatientMedications_Antidepressants", format = "parquet")

lookup_df <- med_recode |>
  dplyr::filter(table == "antipsychotics") |>
  dplyr::filter(Category_Level_3 == "ANTIPSYCHOTICS") |>
  dplyr::filter(canonical_name  != "remove") |>
  mutate(Category_Level_5 = "Other Antipsychotics") |>
  dplyr::select(SimpleGenericName = raw_name, canonical_name_int, 
                Category_Level_5_int, Category_Level_3_int)

open_dataset(
  "Z:/Project D91201F/PatientMedications_Antipsychotics.csv",
  format = "csv",
  convert_options = CsvConvertOptions$create(
    null_values = c("", "NA", "NULL", "null", "*Unspecified"),
    strings_can_be_null = TRUE
  )
) |>
  filter(ExposureLabel %in% c("Antipsychotic", "Misc. Psychotherapeutic")) |>
  dplyr::select(-ExposureLabel) |>
  left_join(lookup_df, by = "SimpleGenericName") |>
  dplyr::filter(!is.na(canonical_name_int)) |>
  dplyr::select(-SimpleGenericName) |>
  dplyr::rename(SimpleGenericName = canonical_name_int) |>
  mutate(DaysSupply = Quantity/DailyDose) |>
  dplyr::select(-Quantity, -DailyDose) |>
  write_dataset("OutputData/PatientMedications_Antipsychotics", format = "parquet")

antidepressant_table <- open_dataset("OutputData/PatientMedications_Antidepressants")

antipsychotics_table <- open_dataset("OutputData/PatientMedications_Antipsychotics")

simp_drug_record <- dplyr::union_all(antidepressant_table, antipsychotics_table) |>
  write_dataset("OutputData/PatientMedications_ADandAP", format = "parquet")


# Version 2 - save as parquet with character columns
library(arrow)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(lubridate)

source("helper_functions.R")

config <- fromJSON("config.json")


atc_drugs <- read_csv(config$files$atc_drugs, 
                      col_types = cols(
                        ATC_code         = col_character(),
                        Name             = col_character(),
                        Category_Level_4 = col_character(),
                        Category_Level_3 = col_character(),
                        Category_Level_2 = col_character(),
                        Category_Level_1 = col_character()
                      )) %>%
  mutate(length = nchar(Name)) %>%
  arrange(Name) %>%
  mutate(Name = as.factor(Name))

drug_class <- read_csv(config$files$drug_class,
                       col_types = cols(
                         Name = col_character(),
                         Type = col_character()
                       ))

med_recode  <- read_csv(config$files$medication_recode, 
                        col_types = cols(
                          table          = col_character(),
                          raw_name       = col_character(),
                          canonical_name = col_character(),
                          subclass       = col_character()
                        )) %>%
  mutate(med_code = as.integer(as.numeric(as.factor(canonical_name)))) %>%
  left_join(atc_drugs, by = join_by(canonical_name == Name)) %>%
  left_join(drug_class, by = join_by(canonical_name == Name)) %>%
  rename(Category_Level_5 = "Type") %>%
  relocate(Category_Level_5, .before = Category_Level_4)

# Build a lookup tibble from your med_recode
lookup_df <- med_recode |>
  dplyr::filter(table == "antidepressant") |>
  dplyr::filter(Category_Level_3 == "ANTIDEPRESSANTS") |>
  dplyr::select(SimpleGenericName = raw_name, canonical_name)#, Category_Level_5, Category_Level_3)

open_dataset(
  "Z:/Project D91201F/PatientMedications_Antidepressants.csv",
  format = "csv",
  convert_options = CsvConvertOptions$create(
    null_values = c("", "NA", "NULL", "null", "*Unspecified"),
    strings_can_be_null = TRUE
  )
) |>
  filter(ExposureLabel %in% c("Antidepressant", "Misc. Psychotherapeutic")) |>
  dplyr::select(-ExposureLabel) |>
  left_join(lookup_df, by = "SimpleGenericName") |>
  dplyr::filter(canonical_name != "remove" & !is.na(canonical_name)) |>
  dplyr::select(-SimpleGenericName) |>
  dplyr::rename(SimpleGenericName = canonical_name) |>
  mutate(DaysSupply = Quantity/DailyDose) |>
  dplyr::select(-Quantity, -DailyDose) |>
  write_dataset("OutputData/PatientMedications_Antidepressants", format = "parquet")

lookup_df <- med_recode |>
  dplyr::filter(table == "antipsychotics") |>
  dplyr::filter(Category_Level_3 == "ANTIPSYCHOTICS") |>
  mutate(Category_Level_5 = "Other Antipsychotics") |>
  dplyr::select(SimpleGenericName = raw_name, canonical_name)# Category_Level_5, Category_Level_3)

open_dataset(
  "Z:/Project D91201F/PatientMedications_Antipsychotics.csv",
  format = "csv",
  convert_options = CsvConvertOptions$create(
    null_values = c("", "NA", "NULL", "null", "*Unspecified"),
    strings_can_be_null = TRUE
  )
) |>
  filter(ExposureLabel %in% c("Antipsychotic", "Misc. Psychotherapeutic")) |>
  dplyr::select(-ExposureLabel) |>
  left_join(lookup_df, by = "SimpleGenericName") |>
  dplyr::filter(canonical_name != "remove" & !is.na(canonical_name)) |>
  dplyr::select(-SimpleGenericName) |>
  dplyr::rename(SimpleGenericName = canonical_name) |>
  mutate(DaysSupply = Quantity/DailyDose) |>
  dplyr::select(-Quantity, -DailyDose) |>
  write_dataset("OutputData/PatientMedications_Antipsychotics", format = "parquet")


# Version 3 - straight conversion of the med tables to parquet

library(arrow)

open_dataset(
  "Z:/Project D91201F/PatientMedications_Antidepressants.csv",
  format = "csv",
  convert_options = CsvConvertOptions$create(
    null_values = c("", "NA", "NULL", "null", "*Unspecified"),
    strings_can_be_null = TRUE
  )
) |>
  write_dataset("OutputData/PatientMedications_Antidepressants", format = "parquet")

open_dataset(
  "Z:/Project D91201F/PatientMedications_Antipsychotics.csv",
  format = "csv",
  convert_options = CsvConvertOptions$create(
    null_values = c("", "NA", "NULL", "null", "*Unspecified"),
    strings_can_be_null = TRUE
  )
) |>
  write_dataset("OutputData/PatientMedications_Antipsychotics", format = "parquet")