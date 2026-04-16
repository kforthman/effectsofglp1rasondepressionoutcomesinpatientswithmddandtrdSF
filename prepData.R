library(tidyverse)
library(jsonlite)

source("helper_functions.R")

config <- fromJSON("config.json")

col_schema  <- read.csv(config$files$column_schema,   stringsAsFactors = FALSE)
med_recode  <- read.csv(config$files$medication_recode, stringsAsFactors = FALSE)

data_pull_date                  <- as.Date(config$data_pull_date)
target_drug                     <- config$target_drug
comparator_drugs                <- config$comparator_drugs
nontreatment_group              <- config$nontreatment_group
eligibility_inclusion_diagnoses <- config$eligibility_inclusion_diagnoses

all_drugs         <- c(target_drug, comparator_drugs)
comparator_groups <- c(nontreatment_group, comparator_drugs)
all_groups        <- c(target_drug, comparator_groups)

atc_drugs <- read.csv(config$files$atc_drugs) %>%
  mutate(length = nchar(Name)) %>%
  arrange(Name)
cpt_acuity  <- read.csv(config$files$cpt_acuity)

# ── Validate column schema against data files ─────────────────────────────────
check_schema(col_schema, "med_table",                  config$files$med_table)
check_schema(col_schema, "diag_table",                 config$files$diag_table)
check_schema(col_schema, "mdd_data",                   config$files$mdd_data)
check_schema(col_schema, "treatment_overlap_table",    config$files$treatment_overlap_table)
check_schema(col_schema, "dte_cohort_data",            config$files$dte_cohort_data)
check_schema(col_schema, "nonswitch_periods",          config$files$nonswitch_periods)
check_schema(col_schema, "psych_proc",                 config$files$psych_proc)
check_schema(col_schema, "encounter_table",            config$files$encounter_table)

# ── Read input data ───────────────────────────────────────────────────────────----

med_table <- read_csv(config$files$med_table,
                      na        = c("", "NA", "NULL", "null"),
                      col_types = make_col_types(col_schema, "med_table")
)

# ── Validate medication recode coverage ───────────────────────────────────────
check_recode(med_table %>% filter(ExposureLabel %in% c("Antidepressants", "Misc. Psychotherapeutic")),
             med_recode, "antidepressant")
check_recode(med_table %>% filter(ExposureLabel  %in% c("Antipsychotics", "Misc. Psychotherapeutic")),
             med_recode, "antipsychotics")
check_recode(med_table %>% filter(ExposureLabel %in% c("Antihypertensive", "Diuretics", "Hydrochlorothiazide")),
             med_recode, "hydrochlorothiazide")
check_recode(med_table %>% filter(ExposureLabel %in% all_drugs),
             med_recode, "treatments")

antidepressant_table <- med_table %>%
  filter(ExposureLabel %in% c("Antidepressants", "Misc. Psychotherapeutic")) %>%
  apply_recode(med_recode, "antidepressant") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 4) == "N06A")
save(antidepressant_table, file = "OutputData/antidepressant_table.rds")

antipsychotics_table <- med_table %>%
  filter(ExposureLabel %in% c("Antipsychotics", "Misc. Psychotherapeutic")) %>%
  apply_recode(med_recode, "antipsychotics") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 5) %in% c("N05AE", "N05AH", "N05AL", "N05AN", "N05AX") & ATC_code != "N05AH02")
save(antipsychotics_table, file = "OutputData/antipsychotics_table.rds")

hydrochlorothiazide_table <- med_table %>%
  filter(ExposureLabel %in% c("Antihypertensive", "Diuretics", "Hydrochlorothiazide")) %>%
  apply_recode(med_recode, "hydrochlorothiazide") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(ATC_code == "C03AA03")
save(hydrochlorothiazide_table, file = "OutputData/hydrochlorothiazide_table.rds")

treatments_subclass_map <- med_recode %>%
  filter(table == "treatments", !is.na(subclass) & subclass != "") %>%
  distinct(canonical_name, subclass)

treatments_table <- med_table %>%
  filter(ExposureLabel %in% all_drugs) %>%
  apply_recode(med_recode, "treatments") %>%
  separate_rows(SimpleGenericName, sep = "/") %>%
  left_join(treatments_subclass_map, by = c("SimpleGenericName" = "canonical_name")) %>%
  mutate(PharmaceuticalSubclass = dplyr::coalesce(subclass, PharmaceuticalSubclass)) %>%
  dplyr::select(-subclass)

rm(treatments_subclass_map)
rm(med_recode)

for(this_drug in all_drugs){
  table_name <- paste0("treatment_", this_drug, "_table")
  table_filename <- paste0("OutputData/treatment_", this_drug,"_table.rds")
  this_table <- treatments_table %>% filter(PharmaceuticalSubclass == this_drug)
  assign(table_name, this_table)
  save(list = table_name, file = table_filename)
  }

rm(med_table)
rm(treatments_table)

diag_table <- read_csv(config$files$diag_table,
                       na        = c("", "NA", "NULL", "null"),
                       col_types = make_col_types(col_schema, "diag_table")
) %>% 
  arrange(PatientDurableKey, Diagnosis)
save(diag_table, file = "OutputData/diag_table.rds")

# Patients with MDD, no Bipolar Disorder, no Schizophrenia
mdd_data <- read_csv(config$files$mdd_data,
                     na        = c("", "NA", "NULL", "null"),
                     col_types = make_col_types(col_schema, "mdd_data")
) %>%
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
  left_join(diag_table %>%
              filter(Diagnosis %in% eligibility_inclusion_diagnoses) %>%
              pivot_wider(names_from = Diagnosis, values_from = FirstDiagnosisDate,
                          values_fill = NA, names_glue = "{Diagnosis}_FirstDiagnosis"),
            by = "PatientDurableKey") %>%
  mutate(across(ends_with("_FirstDiagnosis"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_FirstDiagnosis$', '', .col)}")) %>%
  left_join(antidepressant_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Antidepressant_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antipsychotics_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Antipsychotic_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(hydrochlorothiazide_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Hydrochlorothiazide_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
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
  this_table_name <- paste0("treatment_", this_drug, "_table")
  this_table <- get(this_table_name)
  this_index <- paste0(this_drug, "_Index")
  mdd_data <- mdd_data %>%
    left_join(this_table %>% 
                group_by(PatientDurableKey) %>% 
                summarize(!!sym(this_index) := min(MedicationStartDate)),
              by = "PatientDurableKey") 
  rm(list = this_table_name)
}
rm(this_table)

mdd_data <- mdd_data %>%
  mutate(across(paste0(all_drugs, "_Index"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_Index$', '_Use', .col)}")) %>%
  mutate(Antidepressant_Use = PatientDurableKey %in% antidepressant_table$PatientDurableKey)

save(mdd_data, file = "OutputData/mdd_data.rds")

rm(diag_table)
rm(antidepressant_table)
rm(antipsychotics_table)
rm(hydrochlorothiazide_table)


treatment_overlap_table <- read_csv(config$files$treatment_overlap_table,
                                       na        = c("", "NA", "NULL", "null"),
                                       col_types = make_col_types(col_schema, "treatment_overlap_table")
)
save(treatment_overlap_table, file = "OutputData/treatment_overlap_table.rds")

dte_cohort_data <- read_csv(config$files$dte_cohort_data,
                            na        = c("", "NA", "NULL", "null"),
                            col_types = make_col_types(col_schema, "dte_cohort_data")
) %>%
  left_join(mdd_data,
            by = "PatientDurableKey") %>%
  filter(meets_diagnosis_eligibility_criteria) %>% 
  left_join(treatment_overlap_table, by = "PatientDurableKey")

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

rm(treatment_overlap_table)
rm(dte_cohort_data)

nonswitch_periods <- read_csv(config$files$nonswitch_periods,
                              col_types = make_col_types(col_schema, "nonswitch_periods")
) %>%
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
rm(mdd_data)

psych_proc <- read_csv(config$files$psych_proc,
                       na        = c("", "NA", "NULL", "null"),
                       col_types = make_col_types(col_schema, "psych_proc")
) %>%
  left_join(cpt_acuity %>% 
              dplyr::select(source_concept_code, level), 
            by = join_by("CPTCode" == "source_concept_code"))
save(psych_proc, file = "OutputData/psych_proc.rds")
rm(psych_proc)

encounter_table <- read_csv(config$files$encounter_table,
                       na        = c("", "NA", "NULL", "null"),
                       col_types = make_col_types(col_schema, "encounter_table")
) %>%
  mutate(StartVisit = as.Date(StartVisit),
         EndVisit   = as.Date(EndVisit)) %>%
  mutate(EndVisit = pmax(EndVisit, StartVisit)) # End visit should not come before start visit.
save(encounter_table, file = "OutputData/encounter_table.rds")
rm(encounter_table)

rm(col_schema)
