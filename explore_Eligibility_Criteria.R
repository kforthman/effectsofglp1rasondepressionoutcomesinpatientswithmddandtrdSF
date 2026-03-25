# %% [markdown]
# pre-requisites:
# - prep_Participant_survey_answers.ipynb
# - get_EHR_Availability_Timeframes_noObs.ipynb
# - get_Diagnoses.ipynb
# - get_Visit_Vars.ipynb
# - get_Antidiabetic_Drug_Exposure.ipynb
# - get_Antidiabetic_Timelines.ipynb
# - get_Diagnosis_Timeline.ipynb

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()
source("helper_functions.R")

# %%
load("participant_survey_answers_collapsed_prep.RData", verbose = T)
#participant_survey_answers_collapsed_prep %>% head %>% print_all_cols

# %%
load("ehr_length_noObs_v2.rds")
ehr_length_tab <- ehr_length
#ehr_length_tab %>% head

# %%
load("data_DTE_diagnoses.rds")
study.data.1 <- this.data

# %%
load("data_DTE_visitVars.rds")
study.data.2 <- this.data

# %%
load("data_DTE_AntidiabeticExposure.rds")
study.data.3 <- this.data

# %%
load("data_DTE_AntidiabeticTimelineVars.rds")
study.data.4 <- this.data

# %%
load("data_DTE_DiagnosisTimelineVars.rds")
study.data.5 <- this.data

# %%
all.data.1 <- participant_survey_answers_collapsed_prep %>%
    full_join(ehr_length_tab, by = "person_id") %>%
    full_join(study.data.1, by = "person_id") %>%
    full_join(study.data.2, by = "person_id") %>%
    full_join(study.data.3, by = "person_id") %>%
    full_join(study.data.4, by = "person_id") %>%
    full_join(study.data.5, by = "person_id")

# %%
# Refine Cohort
all.data.2 <- all.data.1 %>% 
filter(person_id %in% ehr_length_tab$person_id) %>%
filter(Age2023 >= 18) %>%
filter(!is.na(age.survey) & !is.na(sex) & !is.na(race.ethnicity)) %>%
filter(!sex == "Other") %>% 
mutate(sex = droplevels(sex))

# %%
cohort_criteria <- "MDD & !BD & !SCH"
all.data.3 <- all.data.2 %>% 
filter(eval(parse(text = cohort_criteria)))

# %%
drug_info <- c("Semaglutide",
                        "Insulins", 
                        "Metformin", 
                        "DPP4i", 
                        "SGLT2i",
                        "SU", 
                        "TZD", 
                        "GLP1RA"
                        ) %>%
matrix(ncol = 1, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug"))

# %%
cohort_names <- do.call(c, lapply(
  drug_info %>% filter(drug != "Semaglutide") %>% pull(drug),
  function(x) {
    c(paste0("Semaglutide_Population_for_Semaglutide_vs_", x), 
      paste0(x, "_Population_for_Semaglutide_vs_", x))
  }
))

# %%
all.data.4 <- all.data.3 %>% 
filter(eval(parse(text = paste(cohort_names, collapse = " | "))))

# %%
dte_cohort_data <- all.data.4

# %%
varsToFactor <- c(
                  "T2DM", 
                  "Obese", 
                  "Hypertension", 
                  "Hypercholesterolemia", 
                  "Hyperlipidemia", 
                  "Heart_Disease", 
                  "Stroke", 
                  "Chronic_Kidney_Disease", 
                  "A1C_over_8p5", 
                  "Pancreatitis", 
                  "T1DM", 
                  "Thyroid_Cancer", 
                  "Gastroparesis", 
                  "T2DM_Overlaps_drug_Index", 
                  "Obese_Overlaps_drug_Index", 
                  "Hypertension_Overlaps_drug_Index", 
                  "Hypercholesterolemia_Overlaps_drug_Index", 
                  "Hyperlipidemia_Overlaps_drug_Index", 
                  "Heart_Disease_Overlaps_drug_Index", 
                  "Stroke_Overlaps_drug_Index", 
                  "Chronic_Kidney_Disease_Overlaps_drug_Index", 
                  "A1C_over_8p5_Overlaps_drug_Index", 
                  "Pancreatitis_Overlaps_drug_Index", 
                  "T1DM_Overlaps_drug_Index", 
                  "Thyroid_Cancer_Overlaps_drug_Index", 
                  "Gastroparesis_Overlaps_drug_Index", 
                  "Semaglutide_Use", 
                  "Insulins_Use", 
                  "Metformin_Use", 
                  "DPP4i_Use", 
                  "SGLT2i_Use", 
                  "SU_Use", 
                  "TZD_Use", 
                  "GLP1RA_Use",
                  "Semaglutide_Overlaps_drug_Index", 
                  "Insulins_Overlaps_drug_Index", 
                  "Metformin_Overlaps_drug_Index", 
                  "DPP4i_Overlaps_drug_Index", 
                  "SGLT2i_Overlaps_drug_Index", 
                  "SU_Overlaps_drug_Index", 
                  "TZD_Overlaps_drug_Index", 
                  "GLP1RA_Overlaps_drug_Index")

new_titles <- c("n", 
                "Diagnoses", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Diagnosis occurs within 6 months before or 12 months after index", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Lifetime Antidiabetic Use", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Co-Occuring Antidiabetic Use within 6 months before or 12 months after index", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "")
new_names <- c("", 
               "Type 2 Diabetes", 
               "Obese", 
               "Hypertension", 
               "Hypercholesterolemia", 
               "Hyperlipidemia", 
               "Heart Disease", 
               "Stroke", 
               "Chronic Kidney Disease", 
               "A1C over 8.5", 
               "Pancreatitis", 
               "Type 1 Diabetes", 
               "Thyroid Cancer", 
               "Gastroparesis", 
               "Type 2 Diabetes", 
               "Obese", 
               "Hypertension", 
               "Hypercholesterolemia", 
               "Hyperlipidemia", 
               "Heart Disease", 
               "Stroke", 
               "Chronic Kidney Disease", 
               "A1C over 8.5", 
               "Pancreatitis", 
               "Type 1 Diabetes", 
               "Thyroid Cancer", 
               "Gastroparesis", 
               "Semaglutide", 
               "Insulins", 
               "Metformin", 
               "DPP-4i", 
               "SGLT-2i", 
               "SU", 
               "TZD", 
               "GLP1-RA (excluding Semaglutide)", 
               "Semaglutide", 
               "Insulins", 
               "Metformin", 
               "DPP-4i", 
               "SGLT-2i", 
               "SU", 
               "TZD", 
               "GLP1-RA (excluding Semaglutide)")

all_diag <-  c("T2DM", "Obese", "Hypertension", "Hypercholesterolemia", "Hyperlipidemia", "Heart_Disease", 
                  "Stroke", "Chronic_Kidney_Disease", "A1C_over_8p5", 
                  "Pancreatitis", "T1DM", "Thyroid_Cancer", "Gastroparesis")

for(this_drug in drug_info$drug){
    
    if(this_drug == "Semaglutide"){next}
    display_markdown(paste("# Semaglutide vs ", this_drug))
    
    Semaglutide_Population_for_Semaglutide_vs_Drug <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    Drug_Population_for_Semaglutide_vs_Drug <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    this.data <- dte_cohort_data %>% 
    filter(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) | !!sym(Drug_Population_for_Semaglutide_vs_Drug))

    for(this_diag in all_diag){
        new_col <- paste0(this_diag, "_Overlaps_drug_Index")
        semaglutide_col <- paste0(this_diag, "_Overlaps_Semaglutide_Index")
        other_col <- paste0(this_diag, "_Overlaps_", this_drug, "_Index")

        this.data <-  this.data %>% 
        mutate(!!new_col := ifelse(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug), !!sym(semaglutide_col), !!sym(other_col)))
    }
    
    for(this_demo_drug in drug_info$drug){
        if(this_demo_drug == "Semaglutide"){
            semaglutide_col <- "Semaglutide_Use"
            other_col <- paste0(this_demo_drug, "_Overlaps_", this_drug, "_Index")
        }else if(this_demo_drug == this_drug){
            semaglutide_col <- paste0(this_demo_drug, "_Overlaps_Semaglutide_Index")
            other_col <- paste0(this_demo_drug, "_Use")
        }else{
            semaglutide_col <- paste0(this_demo_drug, "_Overlaps_Semaglutide_Index")
            other_col <- paste0(this_demo_drug, "_Overlaps_", this_drug, "_Index")
        }
        
        new_col <- paste0(this_demo_drug, "_Overlaps_drug_Index")
        
        this.data <-  this.data %>% 
        mutate(!!new_col := ifelse(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug), !!sym(semaglutide_col), !!sym(other_col)))
    }

    my_table1(this.data = this.data, 
              my_strata = Drug_Population_for_Semaglutide_vs_Drug, 
              filename = paste0("Semaglutide_vs_", this_drug, "-Eligibility_Criteria"), 
              varsToFactor = varsToFactor,   
              new_colnames = c("Variable", "", paste0("TOTAL Semaglutide vs ", this_drug, " Population"), "Semaglutide Population", paste0(this_drug, " Population"), "p"),
              verbose = FALSE,
              new_titles = new_titles,
              new_names = new_names)
}
