library(rmarkdown)
library(tidyverse)
library(doParallel)

target_drug      <- "Semaglutide"
comparator_drugs <- c("DPP4i", "GLP1RA", "Insulins", "Metformin",
                      "Nontreatment", "SGLT2i", "SU", "TZD")

# ── Read input data ───────────────────────────────────────────────────────────

mdd_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260318-1500/MDDPatientList_Table-26_03_18-v1.csv",
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
                       HasInclA             = col_logical(),
                       HasOrInclA           = col_logical(),
                       HasInclB             = col_logical(),
                       HasOrInclB           = col_logical(),
                       HasExclA             = col_logical(),
                       HasExclB             = col_logical(),
                       HasExclC             = col_logical(),
                       BirthDate            = col_date(format = "%Y-%m-%d")
                     )) %>%
  mutate(First_Patient_Record = as.Date(First_Patient_Record),
         Last_Patient_Record  = as.Date(Last_Patient_Record))

dte_cohort_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260318-1500/Treatment_Table-26_03_18-v1.csv",
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
         Semaglutide_vs_DPP4i_AllCriteriaMet  = "Semaglutide_vs_DPP-4i_AllCriteriaMet",
         Semaglutide_vs_GLP1RA_AllCriteriaMet = "Semaglutide_vs_GLP-1RA_AllCriteriaMet",
         DPP4i_vs_Semaglutide_AllCriteriaMet  = "DPP-4i_vs_Semaglutide_AllCriteriaMet",
         GLP1RA_vs_Semaglutide_AllCriteriaMet = "GLP-1RA_vs_Semaglutide_AllCriteriaMet") %>%
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, Eligibility_Group_B),
            by = "PatientDurableKey") %>%
  filter(Eligibility_Group_B)

nonswitch_periods <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260318-1500/Nontreatment_Table-26_03_18-v1.csv",
                              col_types = cols(
                                PatientDurableKey = col_character(),
                                StartConcept      = col_character(),
                                StartDate         = col_date(format = "%Y-%m-%d %H:%M:%S"),
                                EndConcept        = col_character(),
                                EndDate           = col_date(format = "%Y-%m-%d %H:%M:%S")
                              )) %>%
  mutate(StartDate = as.Date(StartDate),
         EndDate   = as.Date(EndDate)) %>%
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, Eligibility_Group_B),
            by = "PatientDurableKey") %>%
  filter(Eligibility_Group_B) %>%
  mutate(
    at_6_months_after_start_date = StartDate + days(180),
    at_12_months_before_end_date = EndDate - days(365),
    tfe_at_index_bgn = floor(time_length(interval(MDD_Index, at_6_months_after_start_date), "days")),
    tfe_at_index_end = floor(time_length(interval(MDD_Index, at_12_months_before_end_date), "days")),
  )

med_table <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260326-1400/CCM-Medication_Table-26_03_26-v1.csv",
                      na = c("", "NA", "NULL", "null"),
                      col_types = cols(
                        PatientEpicId_SH         = col_character(),
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
                                    "Sertraline HCl" = "sertraline",
                                    "DULoxetine HCl" = "duloxetine",
                                    "PARoxetine HCl" = "paroxetine",
                                    "Mirtazapine" = "mirtazapine",
                                    "buPROPion HCl" = "bupropion",
                                    "Escitalopram Oxalate" = "escitalopram",
                                    "Venlafaxine HCl" = "venlafaxine",
                                    "traZODone HCl" = "trazodone",
                                    "FLUoxetine HCl" = "fluoxetine",
                                    "Nortriptyline HCl" = "nortriptyline",
                                    "Amitriptyline HCl" = "amitriptyline",
                                    "Citalopram Hydrobromide" = "citalopram",
                                    "Doxepin HCl" = "doxepin",
                                    "Desvenlafaxine Succinate" = "desvenlafaxine",
                                    "Vilazodone HCl" = "vilazodone",
                                    "Vortioxetine HBr" = "vortioxetine",
                                    "Levomilnacipran HCl" = "levomilnacipran",
                                    "Imipramine HCl" = "imipramine",
                                    "fluvoxaMINE Maleate" = "fluvoxamine",
                                    "clomiPRAMINE HCl" = "clomipramine",
                                    "Dextromethorphan-Bupropion" = "bupropion",
                                    "Nefazodone HCl" = "nefazodone",
                                    "Desipramine HCl" = "desipramine",
                                    "Esketamine HCl" = "esketamine",
                                    "Selegiline" = "selegiline",
                                    "PARoxetine Mesylate" = "paroxetine",
                                    "Imipramine Pamoate" = "imipramine",
                                    "Desvenlafaxine" = "desvenlafaxine",
                                    "Zuranolone" = "zuranolone",
                                    "buPROPion HBr" = "bupropion",
                                    "Phenelzine Sulfate" = "phenelzine",
                                    "Tranylcypromine Sulfate" = "tranylcypromine",
                                    "Desvenlafaxine Fumarate" = "desvenlafaxine",
                                    "Amoxapine" = "amoxapine",
                                    "TraZODone & Diet Manage Prod" = "trazodone"
  )) %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 4) == "N06A")

antipsychotics_table <- med_table %>% filter(PharmaceuticalClass == "Antipsychotics") %>%
  mutate(SimpleGenericName = recode(SimpleGenericName,
                                    "Prochlorperazine" = "prochlorperazine",
                                    "Cariprazine HCl" = "cariprazine",
                                    "Prochlorperazine Edisylate" = "prochlorperazine",
                                    "QUEtiapine Fumarate" = "quetiapine",
                                    "Lurasidone HCl" = "lurasidone",
                                    "risperiDONE" = "risperidone",
                                    "ARIPiprazole" = "aripiprazole",
                                    "Haloperidol Lactate" = "haloperidol",
                                    "OLANZapine" = "olanzapine",
                                    "Haloperidol" = "haloperidol",
                                    "chlorproMAZINE HCl" = "chlorpromazine",
                                    "Prochlorperazine Maleate" = "prochlorperazine",
                                    "Asenapine Maleate" = "asenapine",
                                    "Lithium Carbonate" = "lithium",
                                    "Brexpiprazole" = "brexpiprazole",
                                    "Paliperidone Palmitate" = "paliperidone",
                                    "Ziprasidone HCl" = "ziprasidone",
                                    "Iloperidone" = "iloperidone",
                                    "Thioridazine HCl" = "thioridazine",
                                    "Pimavanserin Tartrate" = "pimavanserin",
                                    "Ziprasidone Mesylate" = "ziprasidone",
                                    "Perphenazine" = "perphenazine",
                                    "Lumateperone Tosylate" = "lumateperone",
                                    "cloZAPine" = "clozapine",
                                    "carBAMazepine (Antipsychotic)" = "carbamazepine",
                                    "fluPHENAZine HCl" = "fluphenazine",
                                    "Paliperidone" = "paliperidone",
                                    "Loxapine Succinate" = "loxapine",
                                    "Haloperidol Decanoate" = "haloperidol",
                                    "risperiDONE Microspheres" = "risperidone",
                                    "Lithium" = "lithium",
                                    "ARIPiprazole (sensor)" = "aripiprazole",
                                    "fluPHENAZine Decanoate" = "fluphenazine",
                                    "Trifluoperazine HCl" = "trifluoperazine")
  ) %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(substr(ATC_code, 1, 5) %in% c("N05AE", "N05AH", "N05AL", "N05AN", "N05AX") & ATC_code != "N05AH02")

hydrochlorothiazide_table <- med_table %>% filter(PharmaceuticalClass == "Antihypertensive" | 
                                                    PharmaceuticalClass == "Diuretics") %>%
  mutate(SimpleGenericName = recode(SimpleGenericName, 
                                    "hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Losartan Potassium-HCTZ" = "hydrochlorothiazide",
                                    "Lisinopril-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Olmesartan Medoxomil-HCTZ" = "hydrochlorothiazide",
                                    "Enalapril-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Bisoprolol-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Valsartan-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Triamterene-HCTZ" = "hydrochlorothiazide",
                                    "Benazepril-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Spironolactone-HCTZ" = "hydrochlorothiazide",
                                    "amLODIPine-Valsartan-HCTZ" = "hydrochlorothiazide",
                                    "Aliskiren-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Olmesartan-amLODIPine-HCTZ" = "hydrochlorothiazide",
                                    "Irbesartan-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Telmisartan-HCTZ" = "hydrochlorothiazide",
                                    "aMILoride-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Eprosartan Mesylate-HCTZ" = "hydrochlorothiazide",
                                    "Moexipril-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Metoprolol-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Candesartan Cilexetil-HCTZ" = "hydrochlorothiazide",
                                    "Fosinopril Sodium-HCTZ" = "hydrochlorothiazide",
                                    "Propranolol-HCTZ" = "hydrochlorothiazide",
                                    "Quinapril-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Methyldopa-hydroCHLOROthiazide" = "hydrochlorothiazide",
                                    "Captopril-hydroCHLOROthiazide" = "hydrochlorothiazide")) %>%
  left_join(atc_drugs, by = join_by("SimpleGenericName" == "Name")) %>%
  filter(ATC_code == "C03AA03")

rm(med_table)

# ── Build antidepressant/antipsychotic treatment timelines ────────────────────

drug_class <- read.csv("Data/DrugClasses.csv")

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

# ── Build nontreatment cohort ─────────────────────────────────────────────────

source("get_Antidiabetic_Nontreatment_Timelines.R")

message("Building nontreatment cohort for: ", target_drug)
nontreat_result <- get_Antidiabetic_Nontreatment_Timelines(mdd_data, dte_cohort_data, nonswitch_periods, target_drug)

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

# ── Run propensity scoring and render reports ─────────────────────────────────

source("analysis_Propensity_Scoring.R")

for (drug in comparator_drugs) {
  message("Running propensity scoring for: ", drug)
  ps_result <- analysis_Propensity_Scoring(drug, target_drug,
                                           "OutputData/dte_cohort_wNontreat_data.rds",
                                           "Data/ps_covariates.csv")
  
  write.csv(ps_result$matchingVars.final,
            paste0("OutputData/PS_Covariates-", drug, ".csv"),
            row.names = FALSE)
  
  weighted.data <- ps_result$weighted.data
  save(weighted.data, file = paste0("OutputData/PS_Weighted_Dataset-", drug, ".rds"))
  
  matched.data <- ps_result$matched.data
  save(matched.data, file = paste0("OutputData/PS_Matched_Dataset-", drug, ".rds"))
  
  result_file <- paste0("OutputData/propensity_scoring_result-", target_drug, "Vs", drug, ".rds")
  saveRDS(ps_result, result_file)
  
  message("Rendering report for: ", drug)
  render(
    input       = "report_Propensity_Scoring.Rmd",
    output_file = paste0("Reports/report_Propensity_Scoring-", target_drug, "Vs", drug, ".html"),
    params      = list(
      target_drug     = target_drug,
      comparator_drug = drug,
      result_file     = result_file
    ),
    envir = new.env()
  )
}
