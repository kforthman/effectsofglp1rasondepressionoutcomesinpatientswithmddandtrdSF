library(tidyverse)
library(jsonlite)

config <- fromJSON("config.json")

# ── Read input data ───────────────────────────────────────────────────────────----

med_table <- read_csv(config$files$med_table,
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

diag_table <- read_csv(config$files$diag_table,
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
mdd_data <- read_csv(config$files$mdd_data,
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


antidiabetic_overlap_table <- read_csv(config$files$antidiabetic_overlap_table,
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

dte_cohort_data <- read_csv(config$files$dte_cohort_data,
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

nonswitch_periods <- read_csv(config$files$nonswitch_periods,
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

psych_proc <- read_csv(config$files$psych_proc,
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