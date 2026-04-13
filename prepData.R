library(tidyverse)
library(jsonlite)

source("helper_functions.R")

config <- fromJSON("config.json")

col_schema  <- read.csv(config$files$column_schema,   stringsAsFactors = FALSE)
med_recode  <- read.csv(config$files$medication_recode, stringsAsFactors = FALSE)

# ── Validate column schema against data files ─────────────────────────────────
check_schema(col_schema, "med_table",                  config$files$med_table)
check_schema(col_schema, "diag_table",                 config$files$diag_table)
check_schema(col_schema, "mdd_data",                   config$files$mdd_data)
check_schema(col_schema, "antidiabetic_overlap_table", config$files$antidiabetic_overlap_table)
check_schema(col_schema, "dte_cohort_data",            config$files$dte_cohort_data)
check_schema(col_schema, "nonswitch_periods",          config$files$nonswitch_periods)
check_schema(col_schema, "psych_proc",                 config$files$psych_proc)

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

# ── Read input data ───────────────────────────────────────────────────────────----

med_table <- read_csv(config$files$med_table,
                      na        = c("", "NA", "NULL", "null"),
                      col_types = make_col_types(col_schema, "med_table")
) %>%
  mutate(DaysSupply = as.integer(DaysSupply)) %>%
  dplyr::select(-AntidiabeticIndexLabel, -AntidiabeticIndexDate)

antidepressant_table <- med_table %>%
  filter(PharmaceuticalClass == "Antidepressants") %>%
  apply_recode(med_recode, "antidepressant") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 4) == "N06A")

antipsychotics_table <- med_table %>%
  filter(PharmaceuticalClass == "Antipsychotics") %>%
  apply_recode(med_recode, "antipsychotics") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 5) %in% c("N05AE", "N05AH", "N05AL", "N05AN", "N05AX") & ATC_code != "N05AH02")

hydrochlorothiazide_table <- med_table %>%
  filter(PharmaceuticalClass == "Antihypertensive" | PharmaceuticalClass == "Diuretics") %>%
  apply_recode(med_recode, "hydrochlorothiazide") %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(ATC_code == "C03AA03")

antidiabetics_subclass_map <- med_recode %>%
  filter(table == "antidiabetics", !is.na(subclass) & subclass != "") %>%
  distinct(canonical_name, subclass)

antidiabetics_table <- med_table %>%
  filter(PharmaceuticalClass == "Antidiabetic") %>%
  apply_recode(med_recode, "antidiabetics") %>%
  separate_rows(SimpleGenericName, sep = "/") %>%
  left_join(antidiabetics_subclass_map, by = c("SimpleGenericName" = "canonical_name")) %>%
  mutate(PharmaceuticalSubclass = dplyr::coalesce(subclass, PharmaceuticalSubclass)) %>%
  dplyr::select(-subclass)

antidiabetics_Semaglutide_table <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "Semaglutide")
antidiabetics_Insulins_table    <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "Insulins")
antidiabetics_Metformin_table   <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "Metformin")
antidiabetics_DPP4i_table       <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "DPP4i")
antidiabetics_SGLT2i_table      <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "SGLT2i")
antidiabetics_SU_table          <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "SU")
antidiabetics_TZD_table         <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "TZD")
antidiabetics_GLP1RA_table      <- antidiabetics_table %>% filter(PharmaceuticalSubclass == "GLP1RA")

rm(med_table)
rm(antidiabetics_table)

diag_table <- read_csv(config$files$diag_table,
                       na        = c("", "NA", "NULL", "null"),
                       col_types = make_col_types(col_schema, "diag_table")
) %>%
  rename(Diagnosis = "EligibilityLabel") %>%
  mutate(Diagnosis = recode(Diagnosis,
                            "Type 1 Diabetes"          = "T1DM",
                            "Type 2 Diabetes Mellitus" = "T2DM",
                            "Depression"               = "MDD",
                            "Heart Disease"            = "Heart_Disease",
                            "Chronic Kidney Disease"   = "Chronic_Kidney_Disease",
                            "A1C"                      = "A1C_over_8p5",
                            "Thyroid Cancer"           = "Thyroid_Cancer"
                            
  )) %>% 
  arrange(PatientDurableKey, Diagnosis)

# Patients with MDD, no Bipolar Disorder, no Schizophrenia
mdd_data <- read_csv(config$files$mdd_data,
                     na        = c("", "NA", "NULL", "null"),
                     col_types = make_col_types(col_schema, "mdd_data")
) %>%
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
  Age = time_length(interval(BirthDate, data_pull_date), "years")
  ) %>%
  dplyr::select(
    -Mo6_Before_MDD_Index,
    -Eligibility_Group_A,
    -Eligibility_Group_C,
    -Eligibility_Group_D,
    -Eligibility_Group_E,
    -HasInclA,
    -HasOrInclA,
    -HasInclB,
    -HasOrInclB,
    -HasExclA,
    -HasExclB,
    -HasExclC,
    -FirstRace,
    -SecondRace,
    -ThirdRace,
    -FourthRace,
    -FifthRace,
    -MultiRacial,
    -Ethnicity
  ) %>%
  filter(PatientSex != "Unknown" & !is.na(PatientSex) & !is.na(Race_Ethnicity)) %>%
  filter(Eligibility_Group_B) %>%
  rename(meets_diagnosis_eligibility_criteria = "Eligibility_Group_B",
         Sex = "PatientSex") %>%
  mutate(Race_Ethnicity_white = Race_Ethnicity == "White or Caucasian",
         Sex_male = Sex == "Male") %>%
  left_join(diag_table %>%
              filter(!Diagnosis %in% c("MDD", "Type_2_Diabetes_Mellitus_with_Complications", "Bariatric_Surgery", "ADHD", "Agoraphobia", "Anxiety_Disorder_NOS", "Generalized_Anxiety", "OCD", "Panic_Disorder", "PTSD", "Social_Anxiety_Disorder", "Alcohol_Abuse", "Alcohol_Dependence", "Cannabis_Abuse", "Cannabis_Dependence", "Cocaine_Abuse", "Cocaine_Dependence", "Opioid_Abuse", "Opioid_Dependence", "Sedative_Abuse", "Sedative_Dependence", "Tobacco_Use_Disorder", "Diseases_of_the_Arteries_Artrioles_and_Capillaries")) %>%
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
  left_join(antidiabetics_DPP4i_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(DPP4i_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antidiabetics_GLP1RA_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(GLP1RA_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antidiabetics_Insulins_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Insulins_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antidiabetics_Metformin_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Metformin_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antidiabetics_Semaglutide_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Semaglutide_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antidiabetics_SGLT2i_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(SGLT2i_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antidiabetics_SU_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(SU_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antidiabetics_TZD_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(TZD_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(antipsychotics_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Antipsychotic_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  left_join(hydrochlorothiazide_table %>% 
              group_by(PatientDurableKey) %>% 
              summarize(Hydrochlorothiazide_Index = min(MedicationStartDate)),
            by = "PatientDurableKey") %>%
  mutate(across(paste0(all_drugs, "_Index"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_Index$', '_Use', .col)}")) %>%
  mutate(Antidepressant_Use = PatientDurableKey %in% antidepressant_table$PatientDurableKey)


antidiabetic_overlap_table <- read_csv(config$files$antidiabetic_overlap_table,
                                       na        = c("", "NA", "NULL", "null"),
                                       col_types = make_col_types(col_schema, "antidiabetic_overlap_table")
) %>%
  rename(DPP4i_Overlaps_Semaglutide_Index = "DPP-4i_Overlaps_Semaglutide_-6m_12m",
         GLP1RA_Overlaps_Semaglutide_Index = "GLP-1RA_Overlaps_Semaglutide_-6m_12m",
         Insulins_Overlaps_Semaglutide_Index = "Insulins_Overlaps_Semaglutide_-6m_12m",
         Metformin_Overlaps_Semaglutide_Index = "Metformin_Overlaps_Semaglutide_-6m_12m",
         SGLT2i_Overlaps_Semaglutide_Index = "SGLT2i_Overlaps_Semaglutide_-6m_12m",
         SU_Overlaps_Semaglutide_Index = "SU_Overlaps_Semaglutide_-6m_12m",
         TZD_Overlaps_Semaglutide_Index = "TZD_Overlaps_Semaglutide_-6m_12m",
         DPP4i_Overlaps_Insulins_Index = "DPP-4i_Overlaps_Insulins_-6m_12m",
         GLP1RA_Overlaps_Insulins_Index = "GLP-1RA_Overlaps_Insulins_-6m_12m",
         Metformin_Overlaps_Insulins_Index = "Metformin_Overlaps_Insulins_-6m_12m",
         Semaglutide_Overlaps_Insulins_Index = "Semaglutide_Overlaps_Insulins_-6m_12m",
         SGLT2i_Overlaps_Insulins_Index = "SGLT2i_Overlaps_Insulins_-6m_12m",
         SU_Overlaps_Insulins_Index = "SU_Overlaps_Insulins_-6m_12m",
         TZD_Overlaps_Insulins_Index = "TZD_Overlaps_Insulins_-6m_12m",
         DPP4i_Overlaps_Metformin_Index = "DPP-4i_Overlaps_Metformin_-6m_12m",
         GLP1RA_Overlaps_Metformin_Index = "GLP-1RA_Overlaps_Metformin_-6m_12m",
         Insulins_Overlaps_Metformin_Index = "Insulins_Overlaps_Metformin_-6m_12m",
         Semaglutide_Overlaps_Metformin_Index = "Semaglutide_Overlaps_Metformin_-6m_12m",
         SGLT2i_Overlaps_Metformin_Index = "SGLT2i_Overlaps_Metformin_-6m_12m",
         SU_Overlaps_Metformin_Index = "SU_Overlaps_Metformin_-6m_12m",
         TZD_Overlaps_Metformin_Index = "TZD_Overlaps_Metformin_-6m_12m",
         GLP1RA_Overlaps_DPP4i_Index = "GLP-1RA_Overlaps_DPP-4i_-6m_12m",
         Insulins_Overlaps_DPP4i_Index = "Insulins_Overlaps_DPP-4i_-6m_12m",
         Metformin_Overlaps_DPP4i_Index = "Metformin_Overlaps_DPP-4i_-6m_12m",
         Semaglutide_Overlaps_DPP4i_Index = "Semaglutide_Overlaps_DPP-4i_-6m_12m",
         SGLT2i_Overlaps_DPP4i_Index = "SGLT2i_Overlaps_DPP-4i_-6m_12m",
         SU_Overlaps_DPP4i_Index = "SU_Overlaps_DPP-4i_-6m_12m",
         TZD_Overlaps_DPP4i_Index = "TZD_Overlaps_DPP-4i_-6m_12m",
         DPP4i_Overlaps_SGLT2i_Index = "DPP-4i_Overlaps_SGLT2i_-6m_12m",
         GLP1RA_Overlaps_SGLT2i_Index = "GLP-1RA_Overlaps_SGLT2i_-6m_12m",
         Insulins_Overlaps_SGLT2i_Index = "Insulins_Overlaps_SGLT2i_-6m_12m",
         Metformin_Overlaps_SGLT2i_Index = "Metformin_Overlaps_SGLT2i_-6m_12m",
         Semaglutide_Overlaps_SGLT2i_Index = "Semaglutide_Overlaps_SGLT2i_-6m_12m",
         SU_Overlaps_SGLT2i_Index = "SU_Overlaps_SGLT2i_-6m_12m",
         TZD_Overlaps_SGLT2i_Index = "TZD_Overlaps_SGLT2i_-6m_12m",
         DPP4i_Overlaps_SU_Index = "DPP-4i_Overlaps_SU_-6m_12m",
         GLP1RA_Overlaps_SU_Index = "GLP-1RA_Overlaps_SU_-6m_12m",
         Insulins_Overlaps_SU_Index = "Insulins_Overlaps_SU_-6m_12m",
         Metformin_Overlaps_SU_Index = "Metformin_Overlaps_SU_-6m_12m",
         Semaglutide_Overlaps_SU_Index = "Semaglutide_Overlaps_SU_-6m_12m",
         SGLT2i_Overlaps_SU_Index = "SGLT2i_Overlaps_SU_-6m_12m",
         TZD_Overlaps_SU_Index = "TZD_Overlaps_SU_-6m_12m",
         DPP4i_Overlaps_TZD_Index = "DPP-4i_Overlaps_TZD_-6m_12m",
         GLP1RA_Overlaps_TZD_Index = "GLP-1RA_Overlaps_TZD_-6m_12m",
         Insulins_Overlaps_TZD_Index = "Insulins_Overlaps_TZD_-6m_12m",
         Metformin_Overlaps_TZD_Index = "Metformin_Overlaps_TZD_-6m_12m",
         Semaglutide_Overlaps_TZD_Index = "Semaglutide_Overlaps_TZD_-6m_12m",
         SGLT2i_Overlaps_TZD_Index = "SGLT2i_Overlaps_TZD_-6m_12m",
         SU_Overlaps_TZD_Index = "SU_Overlaps_TZD_-6m_12m",
         DPP4i_Overlaps_GLP1RA_Index = "DPP-4i_Overlaps_GLP-1RA_-6m_12m",
         Insulins_Overlaps_GLP1RA_Index = "Insulins_Overlaps_GLP-1RA_-6m_12m",
         Metformin_Overlaps_GLP1RA_Index = "Metformin_Overlaps_GLP-1RA_-6m_12m",
         Semaglutide_Overlaps_GLP1RA_Index = "Semaglutide_Overlaps_GLP-1RA_-6m_12m",
         SGLT2i_Overlaps_GLP1RA_Index = "SGLT2i_Overlaps_GLP-1RA_-6m_12m",
         SU_Overlaps_GLP1RA_Index = "SU_Overlaps_GLP-1RA_-6m_12m",
         TZD_Overlaps_GLP1RA_Index = "TZD_Overlaps_GLP-1RA_-6m_12m")

dte_cohort_data <- read_csv(config$files$dte_cohort_data,
                            na        = c("", "NA", "NULL", "null"),
                            col_types = make_col_types(col_schema, "dte_cohort_data")
) %>%
  rename(DPP4i_Exposure                       = "DPP-4i_Exposure",
         DPP4i_Index                          = "DPP-4i_Index",
         DPP4i_Iplus15                        = "DPP-4i_Iplus15",
         DPP4i_Iplus365                       = "DPP-4i_Iplus365",
         MDD_before_DPP4i                     = "MDD_before_DPP-4i",
         DPP4i_PreIndexEncounterCount         = "DPP-4i_PreIndexEncounterCount",
         DPP4i_PostIndexEncounterCount        = "DPP-4i_PostIndexEncounterCount",
         DPP4i_PrePostInclusionCriteriaMet    = "DPP-4i_PrePostInclusionCriteriaMet",
         GLP1RA_Exposure                      = "GLP-1RA_Exposure",
         GLP1RA_Index                         = "GLP-1RA_Index",
         GLP1RA_Iplus15                       = "GLP-1RA_Iplus15",
         GLP1RA_Iplus365                      = "GLP-1RA_Iplus365",
         MDD_before_GLP1RA                    = "MDD_before_GLP-1RA",
         GLP1RA_PreIndexEncounterCount        = "GLP-1RA_PreIndexEncounterCount",
         GLP1RA_PostIndexEncounterCount       = "GLP-1RA_PostIndexEncounterCount",
         GLP1RA_PrePostInclusionCriteriaMet   = "GLP-1RA_PrePostInclusionCriteriaMet",
         
         Semaglutide_Population_for_Semaglutide_vs_Insulins = "Semaglutide_vs_Insulins_AllCriteriaMet",
         Semaglutide_Population_for_Semaglutide_vs_DPP4i = "Semaglutide_vs_DPP-4i_AllCriteriaMet",
         Semaglutide_Population_for_Semaglutide_vs_GLP1RA = "Semaglutide_vs_GLP-1RA_AllCriteriaMet",
         Semaglutide_Population_for_Semaglutide_vs_Metformin = "Semaglutide_vs_Metformin_AllCriteriaMet",
         Semaglutide_Population_for_Semaglutide_vs_SGLT2i = "Semaglutide_vs_SGLT2i_AllCriteriaMet",
         Semaglutide_Population_for_Semaglutide_vs_SU = "Semaglutide_vs_SU_AllCriteriaMet",
         Semaglutide_Population_for_Semaglutide_vs_TZD = "Semaglutide_vs_TZD_AllCriteriaMet",
         Insulins_Population_for_Semaglutide_vs_Insulins = "Insulins_vs_Semaglutide_AllCriteriaMet",
         DPP4i_Population_for_Semaglutide_vs_DPP4i = "DPP-4i_vs_Semaglutide_AllCriteriaMet",
         GLP1RA_Population_for_Semaglutide_vs_GLP1RA = "GLP-1RA_vs_Semaglutide_AllCriteriaMet",
         Metformin_Population_for_Semaglutide_vs_Metformin = "Metformin_vs_Semaglutide_AllCriteriaMet",
         SGLT2i_Population_for_Semaglutide_vs_SGLT2i = "SGLT2i_vs_Semaglutide_AllCriteriaMet",
         SU_Population_for_Semaglutide_vs_SU = "SU_vs_Semaglutide_AllCriteriaMet",
         TZD_Population_for_Semaglutide_vs_TZD = "TZD_vs_Semaglutide_AllCriteriaMet"
  ) %>%
  mutate(
    Semaglutide_meets_timeline_criteria = Semaglutide_PrePostInclusionCriteriaMet & MDD_before_Semaglutide,
    Insulins_meets_timeline_criteria = Insulins_PrePostInclusionCriteriaMet & MDD_before_Insulins,
    DPP4i_meets_timeline_criteria = DPP4i_PrePostInclusionCriteriaMet & MDD_before_DPP4i,
    GLP1RA_meets_timeline_criteria = GLP1RA_PrePostInclusionCriteriaMet & MDD_before_GLP1RA,
    Metformin_meets_timeline_criteria = Metformin_PrePostInclusionCriteriaMet & MDD_before_Metformin,
    SGLT2i_meets_timeline_criteria = SGLT2i_PrePostInclusionCriteriaMet & MDD_before_SGLT2i,
    SU_meets_timeline_criteria = SU_PrePostInclusionCriteriaMet & MDD_before_SU,
    TZD_meets_timeline_criteria = TZD_PrePostInclusionCriteriaMet & MDD_before_TZD
  ) %>%
  dplyr::select(
    -Semaglutide_Index,
    -Semaglutide_Exposure,
    -Semaglutide_Iplus15,
    -Semaglutide_Iplus365,
    -MDD_before_Semaglutide,
    -Semaglutide_PrePostInclusionCriteriaMet,
    -Insulins_Index,
    -Insulins_Exposure,
    -Insulins_Iplus15,
    -Insulins_Iplus365,
    -MDD_before_Insulins,
    -Insulins_PrePostInclusionCriteriaMet,
    -DPP4i_Index,
    -DPP4i_Exposure,
    -DPP4i_Iplus15,
    -DPP4i_Iplus365,
    -MDD_before_DPP4i,
    -DPP4i_PrePostInclusionCriteriaMet,
    -GLP1RA_Index,
    -GLP1RA_Exposure,
    -GLP1RA_Iplus15,
    -GLP1RA_Iplus365,
    -MDD_before_GLP1RA,
    -GLP1RA_PrePostInclusionCriteriaMet,
    -Metformin_Index,
    -Metformin_Exposure,
    -Metformin_Iplus15,
    -Metformin_Iplus365,
    -MDD_before_Metformin,
    -Metformin_PrePostInclusionCriteriaMet,
    -SGLT2i_Index,
    -SGLT2i_Exposure,
    -SGLT2i_Iplus15,
    -SGLT2i_Iplus365,
    -MDD_before_SGLT2i,
    -SGLT2i_PrePostInclusionCriteriaMet,
    -SU_Index,
    -SU_Exposure,
    -SU_Iplus15,
    -SU_Iplus365,
    -MDD_before_SU,
    -SU_PrePostInclusionCriteriaMet,
    -TZD_Index,
    -TZD_Exposure,
    -TZD_Iplus15,
    -TZD_Iplus365,
    -MDD_before_TZD,
    -TZD_PrePostInclusionCriteriaMet
  ) %>%
  left_join(mdd_data,
            by = "PatientDurableKey") %>%
  filter(meets_diagnosis_eligibility_criteria) %>% 
  left_join(antidiabetic_overlap_table, by = "PatientDurableKey")

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

rm(antidiabetic_overlap_table)

nonswitch_periods <- read_csv(config$files$nonswitch_periods,
                              col_types = make_col_types(col_schema, "nonswitch_periods")
) %>%
  mutate(StartDate = as.Date(StartDate),
         EndDate   = as.Date(EndDate)) %>%
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, meets_diagnosis_eligibility_criteria),
            by = "PatientDurableKey") %>%
  filter(meets_diagnosis_eligibility_criteria) %>%
  mutate(
    at_6_months_after_start_date = StartDate + days(180),
    at_12_months_before_end_date = EndDate - days(365),
    tfe_at_index_bgn = floor(time_length(interval(MDD_Index, at_6_months_after_start_date), "days")),
    tfe_at_index_end = floor(time_length(interval(MDD_Index, at_12_months_before_end_date), "days")),
  )

psych_proc <- read_csv(config$files$psych_proc,
                       na        = c("", "NA", "NULL", "null"),
                       col_types = make_col_types(col_schema, "psych_proc")
) %>%
  left_join(cpt_acuity %>% 
              dplyr::select(source_concept_code, level), 
            by = join_by("CPTCode" == "source_concept_code"))
save(psych_proc, file = "OutputData/psych_proc.rds")