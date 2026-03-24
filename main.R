library(rmarkdown)
library(tidyverse)

target_drug      <- "Semaglutide"
comparator_drugs <- c("DPP4i", "GLP1RA", "Insulins", "Metformin",
                      "Nontreatment", "SGLT2i", "SU", "TZD")

# ── Read input data ───────────────────────────────────────────────────────────

mdd_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260318-1500/MDDPatientList_Table-26_03_18-v1.csv",
                     na = c("", "NA", "NULL", "null"),
                     col_types = cols(
                         PatientDurableKey    = col_character(),
                         MDD_Index            = col_datetime(format = "%Y-%m-%d"),
                         Mo6_Before_MDD_Index = col_datetime(format = "%Y-%m-%d"),
                         Eligibility_Group_A  = col_logical(),
                         Eligibility_Group_B  = col_logical(),
                         Eligibility_Group_C  = col_logical(),
                         Eligibility_Group_D  = col_logical(),
                         Eligibility_Group_E  = col_logical(),
                         First_Patient_Record = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                         Last_Patient_Record  = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                         HasInclA             = col_logical(),
                         HasOrInclA           = col_logical(),
                         HasInclB             = col_logical(),
                         HasOrInclB           = col_logical(),
                         HasExclA             = col_logical(),
                         HasExclB             = col_logical(),
                         HasExclC             = col_logical(),
                         BirthDate            = col_datetime(format = "%Y-%m-%d")
                     )) %>%
    mutate(First_Patient_Record = as.Date(First_Patient_Record),
           Last_Patient_Record  = as.Date(Last_Patient_Record))

dte_cohort_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260318-1500/Treatment_Table-26_03_18-v1.csv",
                             na = c("", "NA", "NULL", "null"),
                             col_types = cols(
                                 PatientDurableKey                          = col_character(),
                                 Semaglutide_Exposure                       = col_logical(),
                                 Semaglutide_Index                          = col_datetime(format = "%Y-%m-%d"),
                                 Semaglutide_Iplus15                        = col_datetime(format = "%Y-%m-%d"),
                                 Semaglutide_Iplus365                       = col_datetime(format = "%Y-%m-%d"),
                                 MDD_before_Semaglutide                     = col_logical(),
                                 Semaglutide_PreIndexEncounterCount         = col_integer(),
                                 Semaglutide_PostIndexEncounterCount        = col_integer(),
                                 Semaglutide_PrePostInclusionCriteriaMet    = col_logical(),
                                 Insulins_Exposure                          = col_logical(),
                                 Insulins_Index                             = col_datetime(format = "%Y-%m-%d"),
                                 Insulins_Iplus15                           = col_datetime(format = "%Y-%m-%d"),
                                 Insulins_Iplus365                          = col_datetime(format = "%Y-%m-%d"),
                                 MDD_before_Insulins                        = col_logical(),
                                 Insulins_PreIndexEncounterCount            = col_integer(),
                                 Insulins_PostIndexEncounterCount           = col_integer(),
                                 Insulins_PrePostInclusionCriteriaMet       = col_logical(),
                                 `DPP-4i_Exposure`                          = col_logical(),
                                 `DPP-4i_Index`                             = col_datetime(format = "%Y-%m-%d"),
                                 `DPP-4i_Iplus15`                           = col_datetime(format = "%Y-%m-%d"),
                                 `DPP-4i_Iplus365`                          = col_datetime(format = "%Y-%m-%d"),
                                 `MDD_before_DPP-4i`                        = col_logical(),
                                 `DPP-4i_PreIndexEncounterCount`            = col_integer(),
                                 `DPP-4i_PostIndexEncounterCount`           = col_integer(),
                                 `DPP-4i_PrePostInclusionCriteriaMet`       = col_logical(),
                                 `GLP-1RA_Exposure`                         = col_logical(),
                                 `GLP-1RA_Index`                            = col_datetime(format = "%Y-%m-%d"),
                                 `GLP-1RA_Iplus15`                          = col_datetime(format = "%Y-%m-%d"),
                                 `GLP-1RA_Iplus365`                         = col_datetime(format = "%Y-%m-%d"),
                                 `MDD_before_GLP-1RA`                       = col_logical(),
                                 `GLP-1RA_PreIndexEncounterCount`           = col_integer(),
                                 `GLP-1RA_PostIndexEncounterCount`          = col_integer(),
                                 `GLP-1RA_PrePostInclusionCriteriaMet`      = col_logical(),
                                 Metformin_Exposure                         = col_logical(),
                                 Metformin_Index                            = col_datetime(format = "%Y-%m-%d"),
                                 Metformin_Iplus15                          = col_datetime(format = "%Y-%m-%d"),
                                 Metformin_Iplus365                         = col_datetime(format = "%Y-%m-%d"),
                                 MDD_before_Metformin                       = col_logical(),
                                 Metformin_PreIndexEncounterCount           = col_integer(),
                                 Metformin_PostIndexEncounterCount          = col_integer(),
                                 Metformin_PrePostInclusionCriteriaMet      = col_logical(),
                                 TZD_Exposure                               = col_logical(),
                                 TZD_Index                                  = col_datetime(format = "%Y-%m-%d"),
                                 TZD_Iplus15                                = col_datetime(format = "%Y-%m-%d"),
                                 TZD_Iplus365                               = col_datetime(format = "%Y-%m-%d"),
                                 MDD_before_TZD                             = col_logical(),
                                 TZD_PreIndexEncounterCount                 = col_integer(),
                                 TZD_PostIndexEncounterCount                = col_integer(),
                                 TZD_PrePostInclusionCriteriaMet            = col_logical(),
                                 SU_Exposure                                = col_logical(),
                                 SU_Index                                   = col_datetime(format = "%Y-%m-%d"),
                                 SU_Iplus15                                 = col_datetime(format = "%Y-%m-%d"),
                                 SU_Iplus365                                = col_datetime(format = "%Y-%m-%d"),
                                 MDD_before_SU                              = col_logical(),
                                 SU_PreIndexEncounterCount                  = col_integer(),
                                 SU_PostIndexEncounterCount                 = col_integer(),
                                 SU_PrePostInclusionCriteriaMet             = col_logical(),
                                 SGLT2i_Exposure                            = col_logical(),
                                 SGLT2i_Index                               = col_datetime(format = "%Y-%m-%d"),
                                 SGLT2i_Iplus15                             = col_datetime(format = "%Y-%m-%d"),
                                 SGLT2i_Iplus365                            = col_datetime(format = "%Y-%m-%d"),
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
                                   StartDate         = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                   EndConcept        = col_character(),
                                   EndDate           = col_datetime(format = "%Y-%m-%d %H:%M:%S")
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

# ── Build nontreatment cohort ─────────────────────────────────────────────────

source("get_Antidiabetic_Nontreatment_Timelines.R")

message("Building nontreatment cohort for: ", target_drug)
nontreat_result <- get_Antidiabetic_Nontreatment_Timelines(mdd_data, dte_cohort_data, nonswitch_periods, target_drug)

this.data <- nontreat_result$dte_cohort_data2
save(this.data, file = "Data/dte_cohort_wNontreat_data.rds")

result_file <- paste0("Data/nontreatment_timelines_result-", target_drug, ".rds")

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
