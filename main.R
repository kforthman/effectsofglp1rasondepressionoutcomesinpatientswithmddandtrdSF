library(rmarkdown)
library(tidyverse)
library(doParallel)
library(corrplot)
library(libRtheme)
library(plotrix)
library(tableone)
library(kableExtra)
library(viridis)
library(caret)
library(twang)
library(MatchIt)
library(ggplot2)
library(ggcorrplot)
library(survey)
library(scales)
library(MASS)
library(performance)
library(DHARMa)
library(sjPlot)

data_pull_date <- as.Date("2026-04-06")
target_drug      <- "Semaglutide"
comparator_drugs <- c("DPP4i", "GLP1RA", "Insulins", "Metformin",
                      "SGLT2i", "SU", "TZD")
all_drugs <- c(target_drug, comparator_drugs)
nontreatment_group <- "Nontreatment"
comparator_groups <- c(nontreatment_group, comparator_drugs)
all_groups <- c(target_drug, comparator_groups)

var_name_to_pretty <- read.csv("Data/var_name_to_pretty.csv")
ps_covariates      <- read.csv("Data/ps_covariates.csv")

eligibility_inclusion_diagnoses <- c("T1DM", 
                                     "T2DM", 
                                     "Obesity", 
                                     "Hypertension", 
                                     "Hypercholesterolemia", 
                                     "Hyperlipidemia", 
                                     "Heart_Disease", 
                                     "Stroke", 
                                     "Chronic_Kidney_Disease", 
                                     "A1C_over_8p5", 
                                     "Pancreatitis", 
                                     "Thyroid_Cancer", 
                                     "Gastroparesis")

atc_drugs <- read.csv("Data/Drug_ATC_Categories.csv") %>% 
  mutate(length = nchar(Name)) %>%
  arrange(Name)

drug_class <- read.csv("Data/DrugClasses.csv")

cpt_acuity <- read.csv("Data/psych_proc_tiers-manualEdit-MPP.csv")

period_info <- read.csv("Data/period_info.csv")

# ── Read input data ───────────────────────────────────────────────────────────----

med_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260406-1400/CCM-Medication_Table-26_04_06-v1.csv",
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

diag_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260406-1400/CCM-FirstDiagnosis_Table-26_04_06-v1.csv",
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
                            
  )) %>% 
  arrange(PatientDurableKey, Diagnosis)

# Patients with MDD, no Bipolar Disorder, no Schizophrenia
mdd_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260406-1400/CCM-MDDPatientList_Table-26_04_06-v1.csv",
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
                       Type_2_Diabetes_Mellitus_with_Complications_FirstDiagnosis = col_date(format = "%Y-%m-%d"),
                       Bariatric_Surgery_FirstDiagnosis       = col_date(format = "%Y-%m-%d"),
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
                       Tobacco_Use_Disorder_FirstDiagnosis    = col_date(format = "%Y-%m-%d"),
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


antidiabetic_overlap_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260406-1400//CCM-OverlapWide_Table-26_04_06-v1.csv",
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

dte_cohort_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260406-1400/CCM-Treatment_Table-26_04_06-v1.csv",
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

nonswitch_periods <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260406-1400/CCM-Nontreatment_Table-26_04_06-v1.csv",
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

psych_proc <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260406-1400/CCM-Outcomes_Table-26_04_06-v1.csv",
                       na       = c("", "NA", "NULL", "null"),
                       col_types = cols(
                         PatientDurableKey = col_character(),
                         OutcomeDate       = col_date(format = "%Y-%m-%d"),
                         OutcomeName       = col_character(),
                         CPTCode           = col_integer()
                       )) %>%
  left_join(cpt_acuity %>% 
              dplyr::select(source_concept_code, level), 
            by = join_by("CPTCode" == "source_concept_code"))
save(psych_proc, file = "OutputData/psych_proc.rds")


# ── Identify TRD patients ─────────────────────────────────────────────────────----

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

# ── Build antidepressant/antipsychotic treatment timelines ────────────────────----

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

# ── Build hydrochlorothiazide treatment timelines ─────────────────────────────----

source("get_Hydrochlorothiazide_Treatment_Timeline.R")

message("Building hydrochlorothiazide treatment timelines")
get_Hydrochlorothiazide_Treatment_Timeline(
  hydrochlorothiazide_table = hydrochlorothiazide_table,
  instance_filename         = "OutputData/hydrochlorothiazide_consecutive_instance.rds",
  overwrite                 = TRUE
)

# ── Build nontreatment cohort ─────────────────────────────────────────────────----

source("get_Antidiabetic_Nontreatment_Timelines.R")

message("Building nontreatment cohort for: ", target_drug)
nontreat_result <- get_Antidiabetic_Nontreatment_Timelines(dte_cohort_data, nonswitch_periods, target_drug, nontreatment_group, mdd_data)

this.data <- nontreat_result$dte_cohort_data2
save(this.data, file = "OutputData/dte_cohort_wNontreat_data.rds")

result_file <- paste0("OutputData/nontreatment_timelines_result-", target_drug, ".rds")

save(nontreat_result, file = result_file)

# ── Render nontreatment timelines report ──────────────────────────────────────----

render(
  input       = "report_Antidiabetic_Nontreatment_Timelines.Rmd",
  output_file = paste0("Reports/report_Antidiabetic_Nontreatment_Timelines-", target_drug, ".html"),
  params      = list(
    target_drug        = target_drug,
    nontreatment_group = nontreatment_group,
    result_file        = result_file
  ),
  envir = new.env()
)

# ── Render antidiabetic overlap report ───────────────────────────────────────----

render(
  input       = "report_Antidiabetic_Overlap.Rmd",
  output_file = paste0("Reports/report_Antidiabetic_Overlap-", target_drug, ".html"),
  params      = list(
    dte_cohort_data2   = nontreat_result$dte_cohort_data2,
    all_groups         = all_groups,
    nontreatment_group = nontreatment_group
  ),
  envir = new.env()
)

# ── Build diagnosis timeline variables ────────────────────────────────────────----

source("get_Diagnosis_Timeline.R")

message("Building diagnosis timeline variables")
get_Diagnosis_Timeline(
  all_drugs        = all_drugs,
  all_diagnoses    = eligibility_inclusion_diagnoses,
  index_dataset    = nontreat_result$dte_cohort_data2,
  output_filename  = "OutputData/data_DTE_DiagnosisTimelineVars.rds"
)

load("OutputData/data_DTE_DiagnosisTimelineVars.rds", verbose = T)
diagnosis_timeline_data <- this.data

# ── Render eligibility criteria report ───────────────────────────────────────----

render(
  input       = "report_Eligibility_Criteria.Rmd",
  output_file = paste0("Reports/report_Eligibility_Criteria-", target_drug, ".html"),
  params      = list(
    eligibility_inclusion_diagnoses = eligibility_inclusion_diagnoses,
    all_drugs                       = all_drugs,
    var_name_to_pretty              = var_name_to_pretty,
    comparator_drugs                = comparator_drugs,
    target_drug                     = target_drug,
    diagnosis_timeline_data         = diagnosis_timeline_data,
    dte_cohort_data2                = nontreat_result$dte_cohort_data2,
    diag_table                      = diag_table,
    nontreatment_group              = nontreatment_group
  ),
  envir = new.env()
)

# ── Render propensity covariates report ──────────────────────────────────────----

render(
  input       = "report_Propensity_Covariates.Rmd",
  output_file = paste0("Reports/report_Propensity_Covariates-", target_drug, ".html"),
  params      = list(
    dte_cohort_data2   = nontreat_result$dte_cohort_data2,
    all_groups         = all_groups,
    target_drug        = target_drug,
    ps_covariates      = ps_covariates,
    var_name_to_pretty = var_name_to_pretty
  ),
  envir = new.env()
)

# ── Run propensity scoring and render reports ─────────────────────────────────----

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
  rm(weighted.data)
  
  matched.data <- ps_result$matched.data
  save(matched.data, file = paste0("OutputData/PS_Matched_Dataset-", group, ".rds"))
  rm(matched.data)
  
  result_file <- paste0("OutputData/propensity_scoring_result-", target_drug, "Vs", group, ".rds")
  save(ps_result, file = result_file)
  rm(ps_result)
  
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

# ── Render PS covariate summary report ───────────────────────────────────────

render(
  input       = "report_PS_Covariate_Summary.Rmd",
  output_file = paste0("Reports/report_PS_Covariate_Summary-", target_drug, ".html"),
  params      = list(
    comparator_groups = comparator_groups,
    ps_covariates     = ps_covariates,
    target_drug       = target_drug,
    output_filename   = paste0("OutputData/PS_Covariates-Summary-", target_drug, ".csv")
  ),
  envir = new.env()
)

# ── Compute outcomes ──────────────────────────────────────────────────────────----

matched_data_files <- setNames(
  paste0("OutputData/PS_Matched_Dataset-", comparator_groups, ".rds"),
  comparator_groups
)

# visits_file      <- "OutputData/dte_cohort_visits.rds"
med_changes_file <- "OutputData/antidepressant_antipsychotic_consecutive_period.rds"
hc_med_file      <- "OutputData/hydrochlorothiazide_consecutive_instance.rds"
psych_proc_file  <- "OutputData/psych_proc.rds"

outcomes_files <- list(
  psych   = paste0("OutputData/outcomes_psych-",         target_drug, ".rds"),
# visits  = paste0("OutputData/outcomes_visits-",        target_drug, ".rds"),
  med     = paste0("OutputData/outcomes_med_changes-",   target_drug, ".rds"),
  hc_med  = paste0("OutputData/outcomes_hc_med_changes-",target_drug, ".rds")
)

source("get_Outcomes_PsychProc.R")
# source("get_Outcomes_Visits.R")
source("get_Outcomes_MedChanges.R")
source("get_Outcomes_HCMedChanges.R")

outcome_tasks <- list(
  list(fn = get_Outcomes_PsychProc,   src_file = psych_proc_file,   out_file = outcomes_files$psych),
# list(fn = get_Outcomes_Visits,      src_file = visits_file,       out_file = outcomes_files$visits),
  list(fn = get_Outcomes_MedChanges,  src_file = med_changes_file,  out_file = outcomes_files$med),
  list(fn = get_Outcomes_HCMedChanges,src_file = hc_med_file,       out_file = outcomes_files$hc_med)
)

n_workers <- min(length(outcome_tasks), max(1L, detectCores(logical = TRUE) - 1L))
cl <- makeCluster(n_workers)
registerDoParallel(cl)

message("Running outcome computations in parallel (", n_workers, " workers)...")
foreach(
  task      = outcome_tasks,
  .packages = c("dplyr", "tidyr", "lubridate", "readr"),
  .export   = c("matched_data_files", "period_info", "target_drug",
                "comparator_groups",
                "get_Outcomes_PsychProc",
              # "get_Outcomes_Visits",
                "get_Outcomes_MedChanges",
                "get_Outcomes_HCMedChanges")
) %dopar% {
  task$fn(task$src_file, matched_data_files, period_info,
          target_drug, comparator_groups,
          task$out_file)
}

stopCluster(cl)
message("All outcome computations complete.")

# ── Join outcome tables and pivot to long format ──────────────────────────────----

load(outcomes_files$psych)
outcomes_psych <- outcomes

# load(outcomes_files$visits)
# outcomes_visits <- outcomes

load(outcomes_files$med)
outcomes_med_changes <- outcomes

load(outcomes_files$hc_med)
outcomes_hc_med_changes <- outcomes

all_outcomes_wide <- outcomes_psych %>%
  # left_join(outcomes_visits,
  #           by = c("PatientDurableKey", "study_cohort", "period")) %>%
  left_join(outcomes_med_changes,
            by = c("PatientDurableKey", "study_cohort", "period")) %>%
  left_join(outcomes_hc_med_changes,
            by = c("PatientDurableKey", "study_cohort", "period"))

all_outcomes <- all_outcomes_wide %>%
  pivot_longer(4:ncol(.), names_to = "var_name", values_to = "value") %>%
  mutate(
    period       = factor(period,       levels = period_info$period),
    study_cohort = factor(study_cohort, levels = paste0(target_drug, " vs ", comparator_groups)),
    var_name     = factor(var_name)
  )

save(all_outcomes, file = paste0("OutputData/all_outcomes-", target_drug, ".rds"))

# ── Negative Binomial Regression analyses ─────────────────────────────────────----

source("analysis_Negative_Binomial_Regression.R")

nb_analyses <- list(
  list(dep_var    = "n_psych_days",
       covariates = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years")),
  list(dep_var    = "n_med_changes",
       covariates = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years"))
)

nb_period_name <- "15 days-12 months after index"

dir.create("Reports", showWarnings = FALSE)

nb_result_files <- character(0)

for (group in comparator_groups) {
  for (analysis in nb_analyses) {
    result_file <- paste0(
      "OutputData/nb_result-", target_drug, "Vs", group,
      "-", analysis$dep_var, "-period", nb_period_name, ".rds"
    )

    message("Fitting NB model: ", target_drug, " vs ", group,
            " | ", analysis$dep_var, " | period ", nb_period_name)

    analysis_Negative_Binomial_Regression(
      matched_data_file = matched_data_files[[group]],
      all_outcomes      = all_outcomes,
      comparator_group  = group,
      target_drug       = target_drug,
      period_name       = nb_period_name,
      dep_var           = analysis$dep_var,
      covariates        = analysis$covariates,
      output_file       = result_file
    )

    nb_result_files <- c(nb_result_files, result_file)

    render(
      input       = "report_Negative_Binomial_Regression.Rmd",
      output_file = paste0("Reports/report_NB-", target_drug, "Vs", group,
                           "-", analysis$dep_var, ".html"),
      params = list(
        result_file      = result_file,
        target_drug      = target_drug,
        comparator_group = group,
        dep_var          = analysis$dep_var
      ),
      envir = new.env()
    )
  }
}

render(
  input       = "report_NB_Summary.Rmd",
  output_file = paste0("Reports/report_NB_Summary-", target_drug, ".html"),
  params = list(
    result_files = nb_result_files,
    target_drug  = target_drug
  ),
  envir = new.env()
)

# ── PWP Gap Time Cox Model analyses ───────────────────────────────────────────----

source("analysis_PWP_Gap_Time_Cox_Model.R")

matched_data_files <- setNames(
  paste0("OutputData/PS_Matched_Dataset-", comparator_groups, ".rds"),
  comparator_groups
)

med_changes_file <- "OutputData/antidepressant_antipsychotic_consecutive_period.rds"
hc_med_file      <- "OutputData/hydrochlorothiazide_consecutive_instance.rds"
psych_proc_file  <- "OutputData/psych_proc.rds"

pwp_period_name <- "15 days-12 months after index"
pwp_period_row  <- period_info[period_info$period == pwp_period_name, ]
pwp_bgn_win     <- pwp_period_row$bgn_win
pwp_end_win     <- pwp_period_row$end_win

pwp_analyses <- list(
  list(dep_var        = "psych_visits",
       event_data_file= psych_proc_file,
       event_date_col = "OutcomeDate",
       dedup_by_day   = TRUE,
       covariates     = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years")),
  list(dep_var        = "med_changes",
       event_data_file= med_changes_file,
       event_date_col = "first_record",
       dedup_by_day   = TRUE,
       covariates     = c("Race_Ethnicity_white", "Sex_male", "age_at_index_years"))
)

pwp_result_files <- character(0)

for (group in comparator_groups) {
  for (analysis in pwp_analyses) {
    result_file <- paste0(
      "OutputData/pwp_result-", target_drug, "Vs", group,
      "-", analysis$dep_var, "-period", pwp_period_name, ".rds"
    )

    message("Fitting PWP model: ", target_drug, " vs ", group,
            " | ", analysis$dep_var, " | period ", pwp_period_name)

    analysis_PWP_Gap_Time_Cox_Model(
      matched_data_file = matched_data_files[[group]],
      event_data_file   = analysis$event_data_file,
      event_date_col    = analysis$event_date_col,
      dedup_by_day      = analysis$dedup_by_day,
      comparator_group  = group,
      target_drug       = target_drug,
      period_name       = pwp_period_name,
      bgn_win           = pwp_bgn_win,
      end_win           = pwp_end_win,
      dep_var           = analysis$dep_var,
      covariates        = analysis$covariates,
      output_file       = result_file
    )

    pwp_result_files <- c(pwp_result_files, result_file)

    render(
      input       = "report_PWP_Gap_Time_Cox_Model.Rmd",
      output_file = paste0("Reports/report_PWP-", target_drug, "Vs", group,
                           "-", analysis$dep_var, ".html"),
      params = list(
        result_file      = result_file,
        target_drug      = target_drug,
        comparator_group = group,
        dep_var          = analysis$dep_var
      ),
      envir = new.env()
    )
  }
}

render(
  input       = "report_PWP_Summary.Rmd",
  output_file = paste0("Reports/report_PWP_Summary-", target_drug, ".html"),
  params = list(
    result_files = pwp_result_files,
    target_drug  = target_drug
  ),
  envir = new.env()
)

