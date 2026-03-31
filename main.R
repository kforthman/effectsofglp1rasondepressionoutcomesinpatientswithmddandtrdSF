library(rmarkdown)
library(tidyverse)
library(doParallel)
library(corrplot)
library(libRtheme)
library(plotrix)

data_pull_date <- as.Date("2026-03-27")
target_drug      <- "Semaglutide"
comparator_drugs <- c("DPP4i", "GLP1RA", "Insulins", "Metformin",
                      "SGLT2i", "SU", "TZD")
all_drugs <- c(target_drug, comparator_drugs)
nontreatment_group <- "Nontreatment"
comparator_groups <- c(nontreatment_group, comparator_drugs)
all_groups <- c(target_drug, comparator_groups)

# ── Read input data ───────────────────────────────────────────────────────────

diag_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260327-1100/CCM-FirstDiagnosis_Table-26_03_27-v1.csv",
                       na = c("", "NA", "NULL", "null"),
                       col_types = cols(
                         PatientDurableKey  = col_character(),
                         EligibilityLabel   = col_character(),
                         FirstDiagnosisDate = col_date(format = "%Y-%m-%d")
                       )
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
                            
  ))

# Patients with MDD, no Bipolar Disorder, no Schizophrenia
mdd_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260327-1600/CCM-MDDPatientList_Table-26_03_27-v2.csv",
                     na = c("", "NA", "NULL", "null"),
                     col_types = cols(
                       PatientDurableKey    = col_character(),
                       MDD_Index            = col_date(format = "%Y-%m-%d"),
                       Mo6_Before_MDD_Index = col_date(format = "%Y-%m-%d"),
                       Eligibility_Group_A  = col_logical(),
                       Eligibility_Group_B  = col_logical(),
                       Eligibility_Group_C  = col_logical(),
                       Eligibility_Group_D  = col_logical(),
                       Eligibility_Group_E  = col_logical(),
                       First_Patient_Record = col_date(format = "%Y-%m-%d %H:%M:%S"),
                       Last_Patient_Record  = col_date(format = "%Y-%m-%d %H:%M:%S"),
                       EHRLengthYrs         = col_double(),
                       TotalEncounters      = col_integer(),
                       AvgYearlyEncounters  = col_double(),
                       TotalOutpatientEncounters         = col_integer(),
                       AvgYearlyOutpatientEncounters     = col_double(),
                       TotalPatientPsychVisits           = col_integer(),
                       AvgYearlyPatientPsychVisits       = col_double(),
                       HasInclA             = col_logical(),
                       HasOrInclA           = col_logical(),
                       HasInclB             = col_logical(),
                       HasOrInclB           = col_logical(),
                       HasExclA             = col_logical(),
                       HasExclB             = col_logical(),
                       HasExclC             = col_logical(),
                       ADHD_FirstDiagnosis                    = col_date(format = "%Y-%m-%d"),
                       Agoraphobia_FirstDiagnosis             = col_date(format = "%Y-%m-%d"),
                       Anxiety_Disorder_NOS_FirstDiagnosis    = col_date(format = "%Y-%m-%d"),
                       Generalized_Anxiety_FirstDiagnosis     = col_date(format = "%Y-%m-%d"),
                       OCD_FirstDiagnosis                     = col_date(format = "%Y-%m-%d"),
                       Panic_Disorder_FirstDiagnosis          = col_date(format = "%Y-%m-%d"),
                       PTSD_FirstDiagnosis                    = col_date(format = "%Y-%m-%d"),
                       Social_Anxiety_Disorder_FirstDiagnosis = col_date(format = "%Y-%m-%d"),
                       Alcohol_Abuse_FirstDiagnosis           = col_date(format = "%Y-%m-%d"),
                       Alcohol_Dependence_FirstDiagnosis      = col_date(format = "%Y-%m-%d"),
                       Cannabis_Abuse_FirstDiagnosis          = col_date(format = "%Y-%m-%d"),
                       Cannabis_Dependence_FirstDiagnosis     = col_date(format = "%Y-%m-%d"),
                       Cocaine_Abuse_FirstDiagnosis           = col_date(format = "%Y-%m-%d"),
                       Cocaine_Dependence_FirstDiagnosis      = col_date(format = "%Y-%m-%d"),
                       Opioid_Abuse_FirstDiagnosis            = col_date(format = "%Y-%m-%d"),
                       Opioid_Dependence_FirstDiagnosis       = col_date(format = "%Y-%m-%d"),
                       Sedative_Abuse_FirstDiagnosis          = col_date(format = "%Y-%m-%d"),
                       Sedative_Dependence_FirstDiagnosis     = col_date(format = "%Y-%m-%d"),
                       Diseases_of_the_Arteries_Artrioles_and_Capillaries_FirstDiagnosis = col_date(format = "%Y-%m-%d"),
                       BirthDate            = col_date(format = "%Y-%m-%d"),
                       PatientSex           = col_character(),
                       FirstRace            = col_character(),
                       SecondRace           = col_character(),
                       ThirdRace            = col_character(),
                       FourthRace           = col_character(),
                       FifthRace            = col_character(),
                       MultiRacial          = col_logical(),
                       Ethnicity            = col_character()
                     )) %>%
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
  rename(meets_diagnosis_eligibility_criteria = "Eligibility_Group_B")

antidiabetic_overlap_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260327-1100/CCM-OverlapWide_Table-26_03_27-v1.csv",
                                       na = c("", "NA", "NULL", "null"),
                                       col_types = cols(PatientDurableKey = col_character(),
                                                        `DPP-4i_Overlaps_Semaglutide_-6m_12m` = col_logical(),
                                                        `GLP-1RA_Overlaps_Semaglutide_-6m_12m` = col_logical(),
                                                        `Insulins_Overlaps_Semaglutide_-6m_12m` = col_logical(),
                                                        `Metformin_Overlaps_Semaglutide_-6m_12m` = col_logical(),
                                                        `SGLT2i_Overlaps_Semaglutide_-6m_12m` = col_logical(),
                                                        `SU_Overlaps_Semaglutide_-6m_12m` = col_logical(),
                                                        `TZD_Overlaps_Semaglutide_-6m_12m` = col_logical(),
                                                        `DPP-4i_Overlaps_Insulins_-6m_12m` = col_logical(),
                                                        `GLP-1RA_Overlaps_Insulins_-6m_12m` = col_logical(),
                                                        `Metformin_Overlaps_Insulins_-6m_12m` = col_logical(),
                                                        `Semaglutide_Overlaps_Insulins_-6m_12m` = col_logical(),
                                                        `SGLT2i_Overlaps_Insulins_-6m_12m` = col_logical(),
                                                        `SU_Overlaps_Insulins_-6m_12m` = col_logical(),
                                                        `TZD_Overlaps_Insulins_-6m_12m` = col_logical(),
                                                        `DPP-4i_Overlaps_Metformin_-6m_12m` = col_logical(),
                                                        `GLP-1RA_Overlaps_Metformin_-6m_12m` = col_logical(),
                                                        `Insulins_Overlaps_Metformin_-6m_12m` = col_logical(),
                                                        `Semaglutide_Overlaps_Metformin_-6m_12m` = col_logical(),
                                                        `SGLT2i_Overlaps_Metformin_-6m_12m` = col_logical(),
                                                        `SU_Overlaps_Metformin_-6m_12m` = col_logical(),
                                                        `TZD_Overlaps_Metformin_-6m_12m` = col_logical(),
                                                        `GLP-1RA_Overlaps_DPP-4i_-6m_12m` = col_logical(),
                                                        `Insulins_Overlaps_DPP-4i_-6m_12m` = col_logical(),
                                                        `Metformin_Overlaps_DPP-4i_-6m_12m` = col_logical(),
                                                        `Semaglutide_Overlaps_DPP-4i_-6m_12m` = col_logical(),
                                                        `SGLT2i_Overlaps_DPP-4i_-6m_12m` = col_logical(),
                                                        `SU_Overlaps_DPP-4i_-6m_12m` = col_logical(),
                                                        `TZD_Overlaps_DPP-4i_-6m_12m` = col_logical(),
                                                        `DPP-4i_Overlaps_SGLT2i_-6m_12m` = col_logical(),
                                                        `GLP-1RA_Overlaps_SGLT2i_-6m_12m` = col_logical(),
                                                        `Insulins_Overlaps_SGLT2i_-6m_12m` = col_logical(),
                                                        `Metformin_Overlaps_SGLT2i_-6m_12m` = col_logical(),
                                                        `Semaglutide_Overlaps_SGLT2i_-6m_12m` = col_logical(),
                                                        `SU_Overlaps_SGLT2i_-6m_12m` = col_logical(),
                                                        `TZD_Overlaps_SGLT2i_-6m_12m` = col_logical(),
                                                        `DPP-4i_Overlaps_SU_-6m_12m` = col_logical(),
                                                        `GLP-1RA_Overlaps_SU_-6m_12m` = col_logical(),
                                                        `Insulins_Overlaps_SU_-6m_12m` = col_logical(),
                                                        `Metformin_Overlaps_SU_-6m_12m` = col_logical(),
                                                        `Semaglutide_Overlaps_SU_-6m_12m` = col_logical(),
                                                        `SGLT2i_Overlaps_SU_-6m_12m` = col_logical(),
                                                        `TZD_Overlaps_SU_-6m_12m` = col_logical(),
                                                        `DPP-4i_Overlaps_TZD_-6m_12m` = col_logical(),
                                                        `GLP-1RA_Overlaps_TZD_-6m_12m` = col_logical(),
                                                        `Insulins_Overlaps_TZD_-6m_12m` = col_logical(),
                                                        `Metformin_Overlaps_TZD_-6m_12m` = col_logical(),
                                                        `Semaglutide_Overlaps_TZD_-6m_12m` = col_logical(),
                                                        `SGLT2i_Overlaps_TZD_-6m_12m` = col_logical(),
                                                        `SU_Overlaps_TZD_-6m_12m` = col_logical(),
                                                        `DPP-4i_Overlaps_GLP-1RA_-6m_12m` = col_logical(),
                                                        `Insulins_Overlaps_GLP-1RA_-6m_12m` = col_logical(),
                                                        `Metformin_Overlaps_GLP-1RA_-6m_12m` = col_logical(),
                                                        `Semaglutide_Overlaps_GLP-1RA_-6m_12m` = col_logical(),
                                                        `SGLT2i_Overlaps_GLP-1RA_-6m_12m` = col_logical(),
                                                        `SU_Overlaps_GLP-1RA_-6m_12m` = col_logical(),
                                                        `TZD_Overlaps_GLP-1RA_-6m_12m` = col_logical())
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

dte_cohort_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260327-1100/CCM-Treatment_Table-26_03_27-v1.csv",
                            na = c("", "NA", "NULL", "null"),
                            col_types = cols(
                              PatientDurableKey                          = col_character(),
                              Semaglutide_Exposure                       = col_logical(),
                              Semaglutide_Index                          = col_date(format = "%Y-%m-%d"),
                              Semaglutide_Iplus15                        = col_date(format = "%Y-%m-%d"),
                              Semaglutide_Iplus365                       = col_date(format = "%Y-%m-%d"),
                              MDD_before_Semaglutide                     = col_logical(),
                              Semaglutide_PreIndexEncounterCount         = col_integer(),
                              Semaglutide_PostIndexEncounterCount        = col_integer(),
                              Semaglutide_PrePostInclusionCriteriaMet    = col_logical(),
                              Insulins_Exposure                          = col_logical(),
                              Insulins_Index                             = col_date(format = "%Y-%m-%d"),
                              Insulins_Iplus15                           = col_date(format = "%Y-%m-%d"),
                              Insulins_Iplus365                          = col_date(format = "%Y-%m-%d"),
                              MDD_before_Insulins                        = col_logical(),
                              Insulins_PreIndexEncounterCount            = col_integer(),
                              Insulins_PostIndexEncounterCount           = col_integer(),
                              Insulins_PrePostInclusionCriteriaMet       = col_logical(),
                              `DPP-4i_Exposure`                          = col_logical(),
                              `DPP-4i_Index`                             = col_date(format = "%Y-%m-%d"),
                              `DPP-4i_Iplus15`                           = col_date(format = "%Y-%m-%d"),
                              `DPP-4i_Iplus365`                          = col_date(format = "%Y-%m-%d"),
                              `MDD_before_DPP-4i`                        = col_logical(),
                              `DPP-4i_PreIndexEncounterCount`            = col_integer(),
                              `DPP-4i_PostIndexEncounterCount`           = col_integer(),
                              `DPP-4i_PrePostInclusionCriteriaMet`       = col_logical(),
                              `GLP-1RA_Exposure`                         = col_logical(),
                              `GLP-1RA_Index`                            = col_date(format = "%Y-%m-%d"),
                              `GLP-1RA_Iplus15`                          = col_date(format = "%Y-%m-%d"),
                              `GLP-1RA_Iplus365`                         = col_date(format = "%Y-%m-%d"),
                              `MDD_before_GLP-1RA`                       = col_logical(),
                              `GLP-1RA_PreIndexEncounterCount`           = col_integer(),
                              `GLP-1RA_PostIndexEncounterCount`          = col_integer(),
                              `GLP-1RA_PrePostInclusionCriteriaMet`      = col_logical(),
                              Metformin_Exposure                         = col_logical(),
                              Metformin_Index                            = col_date(format = "%Y-%m-%d"),
                              Metformin_Iplus15                          = col_date(format = "%Y-%m-%d"),
                              Metformin_Iplus365                         = col_date(format = "%Y-%m-%d"),
                              MDD_before_Metformin                       = col_logical(),
                              Metformin_PreIndexEncounterCount           = col_integer(),
                              Metformin_PostIndexEncounterCount          = col_integer(),
                              Metformin_PrePostInclusionCriteriaMet      = col_logical(),
                              TZD_Exposure                               = col_logical(),
                              TZD_Index                                  = col_date(format = "%Y-%m-%d"),
                              TZD_Iplus15                                = col_date(format = "%Y-%m-%d"),
                              TZD_Iplus365                               = col_date(format = "%Y-%m-%d"),
                              MDD_before_TZD                             = col_logical(),
                              TZD_PreIndexEncounterCount                 = col_integer(),
                              TZD_PostIndexEncounterCount                = col_integer(),
                              TZD_PrePostInclusionCriteriaMet            = col_logical(),
                              SU_Exposure                                = col_logical(),
                              SU_Index                                   = col_date(format = "%Y-%m-%d"),
                              SU_Iplus15                                 = col_date(format = "%Y-%m-%d"),
                              SU_Iplus365                                = col_date(format = "%Y-%m-%d"),
                              MDD_before_SU                              = col_logical(),
                              SU_PreIndexEncounterCount                  = col_integer(),
                              SU_PostIndexEncounterCount                 = col_integer(),
                              SU_PrePostInclusionCriteriaMet             = col_logical(),
                              SGLT2i_Exposure                            = col_logical(),
                              SGLT2i_Index                               = col_date(format = "%Y-%m-%d"),
                              SGLT2i_Iplus15                             = col_date(format = "%Y-%m-%d"),
                              SGLT2i_Iplus365                            = col_date(format = "%Y-%m-%d"),
                              MDD_before_SGLT2i                          = col_logical(),
                              SGLT2i_PreIndexEncounterCount              = col_integer(),
                              SGLT2i_PostIndexEncounterCount             = col_integer(),
                              SGLT2i_PrePostInclusionCriteriaMet         = col_logical(),
                              Semaglutide_vs_Insulins_AllCriteriaMet     = col_logical(),
                              `Semaglutide_vs_DPP-4i_AllCriteriaMet`     = col_logical(),
                              `Semaglutide_vs_GLP-1RA_AllCriteriaMet`    = col_logical(),
                              Semaglutide_vs_Metformin_AllCriteriaMet    = col_logical(),
                              Semaglutide_vs_SGLT2i_AllCriteriaMet       = col_logical(),
                              Semaglutide_vs_SU_AllCriteriaMet           = col_logical(),
                              Semaglutide_vs_TZD_AllCriteriaMet          = col_logical(),
                              Insulins_vs_Semaglutide_AllCriteriaMet     = col_logical(),
                              `DPP-4i_vs_Semaglutide_AllCriteriaMet`     = col_logical(),
                              `GLP-1RA_vs_Semaglutide_AllCriteriaMet`    = col_logical(),
                              Metformin_vs_Semaglutide_AllCriteriaMet    = col_logical(),
                              SGLT2i_vs_Semaglutide_AllCriteriaMet       = col_logical(),
                              SU_vs_Semaglutide_AllCriteriaMet           = col_logical(),
                              TZD_vs_Semaglutide_AllCriteriaMet          = col_logical()
                            )) %>%
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
    -Semaglutide_Exposure,
    -Semaglutide_Iplus15,
    -Semaglutide_Iplus365,
    -MDD_before_Semaglutide,
    -Semaglutide_PrePostInclusionCriteriaMet,
    -Insulins_Exposure,
    -Insulins_Iplus15,
    -Insulins_Iplus365,
    -MDD_before_Insulins,
    -Insulins_PrePostInclusionCriteriaMet,
    -DPP4i_Exposure,
    -DPP4i_Iplus15,
    -DPP4i_Iplus365,
    -MDD_before_DPP4i,
    -DPP4i_PrePostInclusionCriteriaMet,
    -GLP1RA_Exposure,
    -GLP1RA_Iplus15,
    -GLP1RA_Iplus365,
    -MDD_before_GLP1RA,
    -GLP1RA_PrePostInclusionCriteriaMet,
    -Metformin_Exposure,
    -Metformin_Iplus15,
    -Metformin_Iplus365,
    -MDD_before_Metformin,
    -Metformin_PrePostInclusionCriteriaMet,
    -SGLT2i_Exposure,
    -SGLT2i_Iplus15,
    -SGLT2i_Iplus365,
    -MDD_before_SGLT2i,
    -SGLT2i_PrePostInclusionCriteriaMet,
    -SU_Exposure,
    -SU_Iplus15,
    -SU_Iplus365,
    -MDD_before_SU,
    -SU_PrePostInclusionCriteriaMet,
    -TZD_Exposure,
    -TZD_Iplus15,
    -TZD_Iplus365,
    -MDD_before_TZD,
    -TZD_PrePostInclusionCriteriaMet
  ) %>%
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, BirthDate, meets_diagnosis_eligibility_criteria),
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
  select(PatientDurableKey, 
         meets_diagnosis_eligibility_criteria, 
         MDD_Index, 
         BirthDate, 
         sort(setdiff(names(.), c("PatientDurableKey", "meets_diagnosis_eligibility_criteria", "MDD_Index", "BirthDate")))
         )

rm(antidiabetic_overlap_table)

nonswitch_periods <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260327-1100/CCM-Nontreatment_Table-26_03_27-v1.csv",
                              col_types = cols(
                                PatientDurableKey = col_character(),
                                StartConcept      = col_character(),
                                StartDate         = col_date(format = "%Y-%m-%d %H:%M:%S"),
                                EndConcept        = col_character(),
                                EndDate           = col_date(format = "%Y-%m-%d %H:%M:%S")
                              )) %>%
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

psych_proc_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260327-1600/CCM-Outcomes_Table-26_03_27-v1.csv",
                             na = c("", "NA", "NULL", "null"),
                             col_types = cols(
                               PatientDurableKey   = col_character(),
                               OutcomeDate         = col_date(format = "%Y-%m-%d"),
                               OutcomeName         = col_character(),
                               CPTCode             = col_character()
                             )
)

med_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260327-1800/CCM-Medication_Table-26_03_27-v2.csv",
                      na = c("", "NA", "NULL", "null"),
                      col_types = cols(
                        PatientDurableKey        = col_character(),
                        AntidiabeticIndexLabel   = col_character(),
                        AntidiabeticIndexDate    = col_date(format = "%Y-%m-%d"),
                        SimpleGenericName        = col_character(),
                        TherapeuticClass         = col_character(),
                        PharmaceuticalClass      = col_character(),
                        PharmaceuticalSubclass   = col_character(),
                        Strength                 = col_character(),
                        Form                     = col_character(),
                        DoseUnit                 = col_character(),
                        MedicationStartDate      = col_date(format = "%Y-%m-%d"),
                        MedicationEndDate        = col_date(format = "%Y-%m-%d"),
                        RefillsWritten           = col_integer(),
                        DaysSupply               = col_double(),
                        Frequency                = col_character(),
                        Route                    = col_character(),
                        Class                    = col_character(),
                        Mode                     = col_character(),
                        Type                     = col_character(),
                        DiscontinueReason        = col_character()
                      )
) %>%
  mutate(DaysSupply = as.integer(DaysSupply)) %>%
  dplyr::select(-AntidiabeticIndexLabel, -AntidiabeticIndexDate)

atc_drugs <- read.csv("Data/Drug_ATC_Categories.csv") %>% 
  mutate(length = nchar(Name)) %>%
  arrange(Name)

antidepressant_table <- med_table %>% 
  filter(PharmaceuticalClass == "Antidepressants") %>%
  mutate(SimpleGenericName = recode(SimpleGenericName,
                                    "Sertraline HCl"               = "sertraline",
                                    "DULoxetine HCl"               = "duloxetine",
                                    "PARoxetine HCl"               = "paroxetine",
                                    "Mirtazapine"                  = "mirtazapine",
                                    "buPROPion HCl"                = "bupropion",
                                    "Escitalopram Oxalate"         = "escitalopram",
                                    "Venlafaxine HCl"              = "venlafaxine",
                                    "traZODone HCl"                = "trazodone",
                                    "FLUoxetine HCl"               = "fluoxetine",
                                    "Nortriptyline HCl"            = "nortriptyline",
                                    "Amitriptyline HCl"            = "amitriptyline",
                                    "Citalopram Hydrobromide"      = "citalopram",
                                    "Doxepin HCl"                  = "doxepin",
                                    "Desvenlafaxine Succinate"     = "desvenlafaxine",
                                    "Vilazodone HCl"               = "vilazodone",
                                    "Vortioxetine HBr"             = "vortioxetine",
                                    "Levomilnacipran HCl"          = "levomilnacipran",
                                    "Imipramine HCl"               = "imipramine",
                                    "fluvoxaMINE Maleate"          = "fluvoxamine",
                                    "clomiPRAMINE HCl"             = "clomipramine",
                                    "Dextromethorphan-Bupropion"   = "bupropion",
                                    "Nefazodone HCl"               = "nefazodone",
                                    "Desipramine HCl"              = "desipramine",
                                    "Esketamine HCl"               = "esketamine",
                                    "Selegiline"                   = "selegiline",
                                    "PARoxetine Mesylate"          = "paroxetine",
                                    "Imipramine Pamoate"           = "imipramine",
                                    "Desvenlafaxine"               = "desvenlafaxine",
                                    "Zuranolone"                   = "zuranolone",
                                    "buPROPion HBr"                = "bupropion",
                                    "Phenelzine Sulfate"           = "phenelzine",
                                    "Tranylcypromine Sulfate"      = "tranylcypromine",
                                    "Desvenlafaxine Fumarate"      = "desvenlafaxine",
                                    "Amoxapine"                    = "amoxapine",
                                    "TraZODone & Diet Manage Prod" = "trazodone"
  )) %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 4) == "N06A")

antipsychotics_table <- med_table %>% filter(PharmaceuticalClass == "Antipsychotics") %>%
  mutate(SimpleGenericName = recode(SimpleGenericName,
                                    "Prochlorperazine"              = "prochlorperazine",
                                    "Cariprazine HCl"               = "cariprazine",
                                    "Prochlorperazine Edisylate"    = "prochlorperazine",
                                    "QUEtiapine Fumarate"           = "quetiapine",
                                    "Lurasidone HCl"                = "lurasidone",
                                    "risperiDONE"                   = "risperidone",
                                    "ARIPiprazole"                  = "aripiprazole",
                                    "Haloperidol Lactate"           = "haloperidol",
                                    "OLANZapine"                    = "olanzapine",
                                    "Haloperidol"                   = "haloperidol",
                                    "chlorproMAZINE HCl"            = "chlorpromazine",
                                    "Prochlorperazine Maleate"      = "prochlorperazine",
                                    "Asenapine Maleate"             = "asenapine",
                                    "Lithium Carbonate"             = "lithium",
                                    "Brexpiprazole"                 = "brexpiprazole",
                                    "Paliperidone Palmitate"        = "paliperidone",
                                    "Ziprasidone HCl"               = "ziprasidone",
                                    "Iloperidone"                   = "iloperidone",
                                    "Thioridazine HCl"              = "thioridazine",
                                    "Pimavanserin Tartrate"         = "pimavanserin",
                                    "Ziprasidone Mesylate"          = "ziprasidone",
                                    "Perphenazine"                  = "perphenazine",
                                    "Lumateperone Tosylate"         = "lumateperone",
                                    "cloZAPine"                     = "clozapine",
                                    "carBAMazepine (Antipsychotic)" = "carbamazepine",
                                    "fluPHENAZine HCl"              = "fluphenazine",
                                    "Paliperidone"                  = "paliperidone",
                                    "Loxapine Succinate"            = "loxapine",
                                    "Haloperidol Decanoate"         = "haloperidol",
                                    "risperiDONE Microspheres"      = "risperidone",
                                    "Lithium"                       = "lithium",
                                    "ARIPiprazole (sensor)"         = "aripiprazole",
                                    "fluPHENAZine Decanoate"        = "fluphenazine",
                                    "Trifluoperazine HCl"           = "trifluoperazine")
  ) %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 5) %in% c("N05AE", "N05AH", "N05AL", "N05AN", "N05AX") & ATC_code != "N05AH02")

hydrochlorothiazide_table <- med_table %>% filter(PharmaceuticalClass == "Antihypertensive" | 
                                                    PharmaceuticalClass == "Diuretics") %>%
  mutate(SimpleGenericName = recode(SimpleGenericName,
                                    "hydroCHLOROthiazide"            = "hydrochlorothiazide",
                                    "Losartan Potassium-HCTZ"        = "hydrochlorothiazide",
                                    "Lisinopril-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Olmesartan Medoxomil-HCTZ"      = "hydrochlorothiazide",
                                    "Enalapril-hydroCHLOROthiazide"  = "hydrochlorothiazide",
                                    "Bisoprolol-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Valsartan-hydroCHLOROthiazide"  = "hydrochlorothiazide",
                                    "Triamterene-HCTZ"               = "hydrochlorothiazide",
                                    "Benazepril-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Spironolactone-HCTZ"            = "hydrochlorothiazide",
                                    "amLODIPine-Valsartan-HCTZ"      = "hydrochlorothiazide",
                                    "Aliskiren-hydroCHLOROthiazide"  = "hydrochlorothiazide",
                                    "Olmesartan-amLODIPine-HCTZ"     = "hydrochlorothiazide",
                                    "Irbesartan-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Telmisartan-HCTZ"               = "hydrochlorothiazide",
                                    "aMILoride-hydroCHLOROthiazide"  = "hydrochlorothiazide",
                                    "Eprosartan Mesylate-HCTZ"       = "hydrochlorothiazide",
                                    "Moexipril-hydroCHLOROthiazide"  = "hydrochlorothiazide",
                                    "Metoprolol-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Candesartan Cilexetil-HCTZ"     = "hydrochlorothiazide",
                                    "Fosinopril Sodium-HCTZ"         = "hydrochlorothiazide",
                                    "Propranolol-HCTZ"               = "hydrochlorothiazide",
                                    "Quinapril-hydroCHLOROthiazide"  = "hydrochlorothiazide",
                                    "Methyldopa-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Captopril-hydroCHLOROthiazide"  = "hydrochlorothiazide")) %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(ATC_code == "C03AA03")

antidiabetics_table <-  med_table %>% filter(PharmaceuticalClass == "Antidiabetic") %>%
  mutate(SimpleGenericName = recode(SimpleGenericName,
                                    "Acarbose"                       = "acarbose",
                                    "Alogliptin Benzoate"            = "alogliptin",
                                    "Alogliptin-metFORMIN HCl"       = "alogliptin/metformin",
                                    "Alogliptin-Pioglitazone"        = "alogliptin/pioglitazone",
                                    "Bexagliflozin"                  = "bexagliflozin",
                                    "Canagliflozin"                  = "canagliflozin",
                                    "Canagliflozin-metFORMIN HCl"    = "canagliflozin/metformin",
                                    "ChlorproPAMIDE"                 = "chlorpropamide",
                                    "Dapagliflozin Prop-metFORMIN"   = "dapagliflozin/metformin",
                                    "Dapagliflozin Propanediol"      = "dapagliflozin",
                                    "Dapagliflozin-sAXagliptin"      = "dapagliflozin/saxagliptin",
                                    "Dulaglutide"                    = "dulaglutide",
                                    "Empagliflozin"                  = "empagliflozin",
                                    "Empagliflozin-Linaglip-Metform" = "empagliflozin/linagliptin/metformin",
                                    "Empagliflozin-linaGLIPtin"      = "empagliflozin/linagliptin",
                                    "Empagliflozin-metFORMIN HCl"    = "empagliflozin/metformin",
                                    "Ertugliflozin L-PyroglutamicAc" = "ertugliflozin",
                                    "Ertugliflozin-metFORMIN HCl"    = "ertugliflozin/metformin",
                                    "Ertugliflozin-SITagliptin"      = "ertugliflozin/sitagliptin",
                                    "Exenatide"                      = "exenatide",
                                    "Glimepiride"                    = "glimepiride",
                                    "glipiZIDE"                      = "glipizide",
                                    "glipiZIDE-metFORMIN HCl"        = "glipizide/metformin",
                                    "glyBURIDE"                      = "glibenclamide",
                                    "glyBURIDE Micronized"           = "glibenclamide",
                                    "glyBURIDE-metFORMIN"            = "glibenclamide/metformin",
                                    "Insulin Aspart"                 = "insulin",
                                    "Insulin Aspart (w/Niacinamide)" = "insulin",
                                    "Insulin Aspart Prot & Aspart"   = "insulin",
                                    "Insulin Degludec"               = "insulin",
                                    "Insulin Degludec-Liraglutide"   = "insulin/liraglutide",
                                    "Insulin Detemir"                = "insulin",
                                    "Insulin Glargine"               = "insulin",
                                    "Insulin Glargine-aglr"          = "insulin",
                                    "Insulin Glargine-Lixisenatide"  = "insulin/lixisenatide",
                                    "Insulin Glargine-yfgn"          = "insulin",
                                    "Insulin Glulisine"              = "insulin",
                                    "Insulin Lispro"                 = "insulin",
                                    "Insulin Lispro Prot & Lispro"   = "insulin",
                                    "Insulin Lispro-aabc"            = "insulin",
                                    "linaGLIPtin"                    = "linagliptin",
                                    "linaGLIPtin-metFORMIN HCl"      = "linagliptin/metformin",
                                    "Liraglutide"                    = "liraglutide",
                                    "Lixisenatide"                   = "lixisenatide",
                                    "metFORMIN HCl"                  = "metformin",
                                    "Miglitol"                       = "miglitol",
                                    "Pioglitazone HCl-Glimepiride"   = "pioglitazone/glimepiride",
                                    "Pioglitazone HCl-metFORMIN HCl" = "pioglitazone/metformin",
                                    "sAXagliptin HCl"                = "saxagliptin",
                                    "sAXagliptin-metFORMIN"          = "saxagliptin/metformin",
                                    "Semaglutide"                    = "semaglutide",
                                    "Sitagliptin"                    = "sitagliptin",
                                    "Sitagliptin Base-Metformin HCl" = "sitagliptin/metformin",
                                    "SITagliptin Phos-metFORMIN HCl" = "sitagliptin/metformin",
                                    "SITagliptin Phosphate"          = "sitagliptin",
                                    "TOLBUTamide"                    = "tolbutamide"
  )
  ) %>%
  separate_rows(
    SimpleGenericName,
    sep = "/"
  ) %>%
  mutate(PharmaceuticalSubclass = case_when(
    SimpleGenericName == "acarbose"       ~ "TZD",
    SimpleGenericName == "alogliptin"     ~ "DPP4i",
    SimpleGenericName == "bexagliflozin"  ~ "SGLT2i",
    SimpleGenericName == "canagliflozin"  ~ "SGLT2i",
    SimpleGenericName == "chlorpropamide" ~ "SU",
    SimpleGenericName == "dapagliflozin"  ~ "SGLT2i",
    SimpleGenericName == "dulaglutide"    ~ "GLP1RA",
    SimpleGenericName == "empagliflozin"  ~ "SGLT2i",
    SimpleGenericName == "ertugliflozin"  ~ "SGLT2i",
    SimpleGenericName == "exenatide"      ~ "GLP1RA",
    SimpleGenericName == "glibenclamide"  ~ "SU",
    SimpleGenericName == "glimepiride"    ~ "SU",
    SimpleGenericName == "glipizide"      ~ "SU",
    SimpleGenericName == "insulin"        ~ "Insulins",
    SimpleGenericName == "linagliptin"    ~ "DPP4i",
    SimpleGenericName == "liraglutide"    ~ "GLP1RA",
    SimpleGenericName == "lixisenatide"   ~ "GLP1RA",
    SimpleGenericName == "metformin"      ~ "Metformin",
    SimpleGenericName == "miglitol"       ~ "TZD",
    SimpleGenericName == "pioglitazone"   ~ "Other",
    SimpleGenericName == "saxagliptin"    ~ "DPP4i",
    SimpleGenericName == "semaglutide"    ~ "Semaglutide",
    SimpleGenericName == "sitagliptin"    ~ "DPP4i",
    SimpleGenericName == "tolbutamide"    ~ "SU"
  ))

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

drug_class <- read.csv("Data/DrugClasses.csv")

# ── Identify TRD patients ─────────────────────────────────────────────────────

source("get_TRD.R")

message("Identifying TRD patients")
get_TRD(
  mdd_data                  = mdd_data,
  antidepressant_table      = antidepressant_table,
  instance_filename         = "OutputData/antidepressant_consecutive_instance.csv",
  period_filename           = "OutputData/antidepressant_consecutive_period.csv",
  period_summ_filename      = "OutputData/antidepressant_consecutive_period_tab_summ.csv",
  period_max_drugs_filename = "OutputData/antidepressant_consecutive_period_maxDrugs.csv",
  trd_ids_filename          = "OutputData/IDs-TRD.csv",
  overwrite                 = TRUE
)

# ── Build antidepressant/antipsychotic treatment timelines ────────────────────

source("get_Antidepressant_Treatment_Timeline.R")

message("Building antidepressant/antipsychotic treatment timelines")
get_Antidepressant_Treatment_Timeline(
  drug_class           = drug_class,
  antidepressant_table = antidepressant_table,
  antipsychotics_table = antipsychotics_table,
  instance_filename    = "OutputData/antidepressant_antipsychotic_consecutive_instance.rds",
  period_filename      = "OutputData/antidepressant_antipsychotic_consecutive_period.rds",
  overwrite            = TRUE
)

# ── Build hydrochlorothiazide treatment timelines ─────────────────────────────

source("get_Hydrochlorothiazide_Treatment_Timeline.R")

message("Building hydrochlorothiazide treatment timelines")
get_Hydrochlorothiazide_Treatment_Timeline(
  hydrochlorothiazide_table = hydrochlorothiazide_table,
  instance_filename         = "OutputData/hydrochlorothiazide_consecutive_instance.rds",
  overwrite                 = TRUE
)

# ── Build nontreatment cohort ─────────────────────────────────────────────────

source("get_Antidiabetic_Nontreatment_Timelines.R")

message("Building nontreatment cohort for: ", target_drug)
nontreat_result <- get_Antidiabetic_Nontreatment_Timelines(dte_cohort_data, nonswitch_periods, target_drug)

this.data <- nontreat_result$dte_cohort_data2
save(this.data, file = "OutputData/dte_cohort_wNontreat_data.rds")

result_file <- paste0("OutputData/nontreatment_timelines_result-", target_drug, ".rds")

saveRDS(nontreat_result, result_file)

# ── Render nontreatment timelines report ──────────────────────────────────────

render(
  input       = "report_Antidiabetic_Nontreatment_Timelines.Rmd",
  output_file = paste0("Reports/report_Antidiabetic_Nontreatment_Timelines-", target_drug, ".html"),
  params      = list(
    target_drug = target_drug,
    result_file = result_file
  ),
  envir = new.env()
)

# ── Build diagnosis timeline variables ────────────────────────────────────────

source("get_Diagnosis_Timeline.R")

message("Building diagnosis timeline variables")
get_Diagnosis_Timeline(
  target_drug      = target_drug,
  comparator_groups = comparator_groups,
  all_diagnoses    = c("T1DM", "T2DM", "Hypertension", "Heart_Disease", "Hyperlipidemia",
                       "Obesity", "Hypercholesterolemia", "Chronic_Kidney_Disease",
                       "A1C_over_8p5", "Pancreatitis", "Stroke", "Thyroid_Cancer", "Gastroparesis"),
  index_dataset    = nontreat_result$dte_cohort_data2,
  diag_table       = diag_table,
  output_filename  = "OutputData/data_DTE_DiagnosisTimelineVars.rds"
)

# ── Run propensity scoring and render reports ─────────────────────────────────

source("analysis_Propensity_Scoring.R")

for (group in comparator_groups) {
  message("Running propensity scoring for: ", group)
  ps_result <- analysis_Propensity_Scoring(group, target_drug,
                                           "OutputData/dte_cohort_wNontreat_data.rds",
                                           "Data/ps_covariates.csv")
  
  write.csv(ps_result$matchingVars.final,
            paste0("OutputData/PS_Covariates-", group, ".csv"),
            row.names = FALSE)
  
  weighted.data <- ps_result$weighted.data
  save(weighted.data, file = paste0("OutputData/PS_Weighted_Dataset-", group, ".rds"))
  
  matched.data <- ps_result$matched.data
  save(matched.data, file = paste0("OutputData/PS_Matched_Dataset-", group, ".rds"))
  
  result_file <- paste0("OutputData/propensity_scoring_result-", target_drug, "Vs", group, ".rds")
  saveRDS(ps_result, result_file)
  
  message("Rendering report for: ", group)
  render(
    input       = "report_Propensity_Scoring.Rmd",
    output_file = paste0("Reports/report_Propensity_Scoring-", target_drug, "Vs", group, ".html"),
    params      = list(
      target_drug     = target_drug,
      comparator_group = group,
      result_file     = result_file
    ),
    envir = new.env()
  )
}
