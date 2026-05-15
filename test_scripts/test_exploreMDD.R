library(arrow)
library(dplyr)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(scales)

config <- fromJSON("config.json")

data_pull_date <- as.Date(config$data_pull_date)
col_schema  <- read.csv(config$files$column_schema,   stringsAsFactors = FALSE)

med_index_table <- open_dataset("Parquet/med_index_table")

# med_index_table %>%
#   head(100) %>%
#   collect() %>%
#   dplyr::glimpse()

diag_table <- open_dataset("Parquet/diag_table") %>%
  mutate(PatientDurableKey = cast(PatientDurableKey, int64())) %>%
  dplyr::select(c("PatientDurableKey",
                  setdiff(col_schema %>%
                            filter(table == "diag_table") %>%
                            pull(column),
                          col_schema %>%
                            filter(table == "mdd_data") %>%
                            pull(column)
                  )))

# diag_table %>%
#   head(100) %>%
#   collect() %>%
#   dplyr::glimpse()

open_dataset("Parquet/mdd_data") %>%
  mutate(PatientDurableKey = cast(PatientDurableKey, int64())) %>%
  left_join(diag_table,
            by = "PatientDurableKey") %>%
  mutate(across(ends_with("_FirstDiagnosis"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_FirstDiagnosis$', '', .col)}")) %>%
  left_join(med_index_table,
            by = "PatientDurableKey") %>%
  mutate(across(ends_with("_Index"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_Index$', '_Use', .col)}")) %>% 
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
  Age = year(data_pull_date) - year(BirthDate)
  ) %>%
  dplyr::select(
    -FirstRace,
    -SecondRace,
    -ThirdRace,
    -FourthRace,
    -FifthRace,
    -MultiRacial,
    -Ethnicity
  ) %>%
  write_dataset(str_glue("Parquet/mdd_data_prepped"), format = "parquet")


mdd_data_prepped <- open_dataset("Parquet/mdd_data_prepped")
# Step 2: Peek at what's actually in the problem columns
mdd_data_prepped %>%
  head(100) %>%
  collect() %>%
  dplyr::glimpse()



out <- mdd_data_prepped %>%
  mutate(eligible = !is.na(Race_Ethnicity) & (Sex %in% c("Male", "Female")) & meets_diagnosis_eligibility_criteria) %>%
  group_by(eligible) %>%
  summarise(total_Semaglutide_User = sum(Semaglutide_Use), 
            total_Insulins_User = sum(Insulins_Use), 
            total_Metformin_User = sum(Metformin_Use), 
            total_DPP4i_User = sum(DPP4i_Use), 
            total_SGLT2i_User = sum(SGLT2i_Use), 
            total_SU_User = sum(SU_Use), 
            total_TZD_User = sum(TZD_Use), 
            total_GLP1RA_User = sum(GLP1RA_Use)) %>%
  collect() %>%
  mutate(across(starts_with("total_"), ~ comma(.x)))

mdd_data_prepped <- open_dataset("Parquet/mdd_data_prepped")
dte_cohort_data <- open_dataset("Parquet/dte_cohort_data")

dte_cohort_data %>%
  mutate(PatientDurableKey = cast(PatientDurableKey, int64())) %>%
  left_join(mdd_data_prepped, by = "PatientDurableKey") %>%
  write_dataset(str_glue("Parquet/dte_cohort_data_prepped"), format = "parquet")

# gc()
# 
# dte_cohort_data %>%
#   head(100) %>%
#   collect() %>%
#   glimpse()
# 
# dte_cohort_data %>%
#   count(Semaglutide_Use)


