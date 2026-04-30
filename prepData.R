library(tidyverse)
library(jsonlite)
library(lubridate)

source("helper_functions.R")

config <- fromJSON("config.json")

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
med_recode  <- read_csv(config$files$medication_recode, 
                        col_types = cols(
                          table          = col_factor(),
                          raw_name       = col_factor(),
                          canonical_name = col_factor(),
                          subclass       = col_factor()
                        ))

data_pull_date                  <- as.Date(config$data_pull_date)
target_drug                     <- config$target_drug
comparator_drugs                <- config$comparator_drugs
nontreatment_group              <- config$nontreatment_group
eligibility_inclusion_diagnoses <- config$eligibility_inclusion_diagnoses

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

# -- Validate column schema ----------------------------------------------------
check_schema_table(col_schema, "med_table_ad",      config, conn = conProjects)
check_schema_table(col_schema, "med_table_ap",      config, conn = conProjects)
# check_schema_table(col_schema, "med_table_hctz",   config, conn = conProjects)
check_schema_table(col_schema, "med_table_treat",   config, conn = conProjects)
check_schema_table(col_schema, "diag_table",        config, conn = conProjects)
check_schema_table(col_schema, "mdd_data",          config, conn = conProjects)
check_schema_table(col_schema, "dte_cohort_data",   config, conn = conProjects)
check_schema_table(col_schema, "nonswitch_periods", config, conn = conProjects)
check_schema_table(col_schema, "psych_proc",        config, conn = conProjects)
check_schema_table(col_schema, "encounter_table",   config, conn = conProjects)

# -- Read input data ---------------------------------------------------------------

antidepressant_table <- read_table(config, col_schema, "med_table_ad",    conn = conProjects)
gc()
check_recode(antidepressant_table %>% filter(ExposureLabel %in% c("Antidepressant", "Misc. Psychotherapeutic")),
             med_recode, "antidepressant")
antidepressant_table <- antidepressant_table %>%
  filter(ExposureLabel %in% c("Antidepressant", "Misc. Psychotherapeutic")) %>%
  apply_recode(med_recode, "antidepressant") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 4) == "N06A")
save(antidepressant_table, file = "OutputData/antidepressant_table.rds")

antidepressant_index <- antidepressant_table %>% 
  group_by(PatientDurableKey) %>% 
  summarize(Antidepressant_Index = min(MedicationStartDate))
rm(antidepressant_table)
gc()

antipsychotics_table <- read_table(config, col_schema, "med_table_ap",    conn = conProjects)
check_recode(antipsychotics_table %>% filter(ExposureLabel  %in% c("Antipsychotic", "Misc. Psychotherapeutic")),
             med_recode, "antipsychotics")
antipsychotics_table <- antipsychotics_table %>%
  filter(ExposureLabel %in% c("Antipsychotic", "Misc. Psychotherapeutic")) %>%
  apply_recode(med_recode, "antipsychotics") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 5) %in% c("N05AE", "N05AH", "N05AL", "N05AN", "N05AX") & ATC_code != "N05AH02")
save(antipsychotics_table, file = "OutputData/antipsychotics_table.rds")

antipsychotics_index <- antipsychotics_table %>% 
  group_by(PatientDurableKey) %>% 
  summarize(Antipsychotic_Index = min(MedicationStartDate))
rm(antipsychotics_table)
gc()

# hydrochlorothiazide_table           <- read_table(config, col_schema, "med_table_hctz",  conn = conProjects)
# check_recode(hydrochlorothiazide_table %>% filter(ExposureLabel %in% c("Hydrochlorothiazide")),
#              med_recode, "hydrochlorothiazide")
# hydrochlorothiazide_table <- hydrochlorothiazide_table %>%
#   filter(ExposureLabel %in% c("Hydrochlorothiazide")) %>%
#   apply_recode(med_recode, "hydrochlorothiazide") %>%
#   left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
#   filter(ATC_code == "C03AA03")
# save(hydrochlorothiazide_table, file = "OutputData/hydrochlorothiazide_table.rds")
# 
# hydrochlorothiazide_index <- hydrochlorothiazide_table %>% 
#   group_by(PatientDurableKey) %>% 
#   summarize(Hydrochlorothiazide_Index = min(MedicationStartDate))
# rm(hydrochlorothiazide_table)
# gc()

treatments_table     <- read_table(config, col_schema, "med_table_treat", conn = conProjects)
check_recode(treatments_table %>% filter(ExposureLabel %in% all_drugs),
             med_recode, "treatments")
treatments_subclass_map <- med_recode %>%
  filter(table == "treatments", !is.na(subclass) & subclass != "") %>%
  distinct(canonical_name, subclass)

treatments_table <- treatments_table %>%
  filter(ExposureLabel %in% all_drugs) %>%
  apply_recode(med_recode, "treatments") %>%
  separate_rows(SimpleGenericName, sep = "/") %>%
  left_join(treatments_subclass_map, by = c("SimpleGenericName" = "canonical_name")) %>%
  mutate(PharmaceuticalSubclass = dplyr::coalesce(subclass, PharmaceuticalSubclass)) %>%
  dplyr::select(-subclass)

rm(treatments_subclass_map)
gc()

for(this_drug in all_drugs){
  this_table_name <- paste0("treatment_", this_drug, "_table")
  table_filename <- paste0("OutputData/treatment_", this_drug,"_table.rds")
  assign(this_table_name, 
         treatments_table %>% 
           filter(PharmaceuticalSubclass == this_drug))
  save(list = this_table_name, file = table_filename)
  rm(this_table_name)
  
  this_index_table_name <- paste0("treatment_", this_drug, "_index")
  this_index <- paste0(this_drug, "_Index")
  assign(this_index_table_name, 
         treatments_table %>% 
           filter(PharmaceuticalSubclass == this_drug) %>% 
           group_by(PatientDurableKey) %>% 
           summarize(!!sym(this_index) := min(MedicationStartDate)))
}

rm(treatments_table)
gc()

diag_table <- read_table(config, col_schema, "diag_table", conn = conProjects) %>%
  arrange(PatientDurableKey)
save(diag_table, file = "OutputData/diag_table.rds")

# Patients with MDD, no Bipolar Disorder, no Schizophrenia
mdd_data <- read_table(config, col_schema, "mdd_data", conn = conProjects) %>%
  rename(Sex = "PatientSex") %>%
  mutate(Race = case_when(
    !is.na(SecondRace) | !is.na(ThirdRace) | !is.na(FourthRace) | !is.na(FifthRace) | MultiRacial ~ "Multi-Race",
    FirstRace == "American Indian or Alaska Native" ~ "American Indian or Alaska Native",
    FirstRace == "Asian" ~ "Asian",
    FirstRace == "Black or African American" ~ "Black or African American",
    FirstRace == "Native Hawaiian or Other Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
    FirstRace == "White or Caucasian" ~ "White or Caucasian"
  ),
  Race_Ethnicity = case_when(
    !is.na(Ethnicity) & Ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
    TRUE ~ Race
  ),
  Race_Ethnicity_white = Race_Ethnicity == "White or Caucasian",
  Sex_male = Sex == "Male",
  Age = time_length(interval(BirthDate, data_pull_date), "years")
  ) %>%
  filter(Sex != "Unknown" & 
           !is.na(Sex) & 
           !is.na(Race_Ethnicity) & 
           meets_diagnosis_eligibility_criteria
  ) %>%
  left_join(diag_table,
            by = "PatientDurableKey") %>%
  mutate(across(ends_with("_FirstDiagnosis"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_FirstDiagnosis$', '', .col)}")) %>%
  left_join(antidepressant_index,
            by = "PatientDurableKey") %>%
  left_join(antipsychotics_index,
            by = "PatientDurableKey") %>%
  # left_join(hydrochlorothiazide_index,
  #           by = "PatientDurableKey") %>%
  dplyr::select(
    -FirstRace,
    -SecondRace,
    -ThirdRace,
    -FourthRace,
    -FifthRace,
    -MultiRacial,
    -Ethnicity
  ) 
  
for(this_drug in all_drugs){
  this_index_table_name <- paste0("treatment_", this_drug, "_index")
  mdd_data <- mdd_data %>%
    left_join(get(this_index_table_name),
              by = "PatientDurableKey") 
  rm(list = this_table_name)
  gc()
}
rm(this_table)
gc()

mdd_data <- mdd_data %>%
  mutate(across(paste0(all_drugs, "_Index"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_Index$', '_Use', .col)}")) %>%
  mutate(Antidepressant_Use = PatientDurableKey %in% antidepressant_index$PatientDurableKey)

save(mdd_data, file = "OutputData/mdd_data.rds")

rm(diag_table)
rm(antidepressant_index)
rm(antipsychotics_index)
# rm(hydrochlorothiazide_index)
gc()

dte_cohort_data <- read_table(config, col_schema, "dte_cohort_data", conn = conProjects) %>%
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
  )

save(dte_cohort_data, file = "OutputData/dte_cohort_data.rds")

rm(dte_cohort_data)
gc()

nonswitch_periods <- read_table(config, col_schema, "nonswitch_periods", conn = conProjects) %>%
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, meets_diagnosis_eligibility_criteria),
            by = "PatientDurableKey") %>%
  filter(meets_diagnosis_eligibility_criteria) %>%
  mutate(
    at_6_months_after_start_date = StartDate + days(180),
    at_12_months_before_end_date = EndDate - days(365),
    tfe_at_index_bgn = floor(time_length(interval(MDD_Index, at_6_months_after_start_date), "days")),
    tfe_at_index_end = floor(time_length(interval(MDD_Index, at_12_months_before_end_date), "days")),
  )
save(nonswitch_periods, file = "OutputData/nonswitch_periods.rds")

rm(nonswitch_periods)
gc()
rm(mdd_data)
gc()

psych_proc <- read_table(config, col_schema, "psych_proc", conn = conProjects) %>%
  left_join(cpt_acuity %>% 
              dplyr::select(source_concept_code, level), 
            by = join_by("CPTCode" == "source_concept_code"))
save(psych_proc, file = "OutputData/psych_proc.rds")
rm(psych_proc)
gc()

encounter_table <- read_table(config, col_schema, "encounter_table", conn = conProjects) %>%
  mutate(StartVisit = as.Date(StartVisit),
         EndVisit   = as.Date(EndVisit)) %>%
  mutate(EndVisit = pmax(EndVisit, StartVisit)) # End visit should not come before start visit.
save(encounter_table, file = "OutputData/encounter_table.rds")
rm(encounter_table)
gc()

rm(col_schema)
gc()

if (!is.null(conProjects)) {
  DBI::dbDisconnect(conProjects)
  rm(conProjects)
  gc()
}
