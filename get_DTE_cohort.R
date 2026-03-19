# %% [markdown]
# pre-requisites:
# - prep_Participant_survey_answers.ipynb
# - get_EHR_Availability_Timeframes_noObs.ipynb
# - get_zip.ipynb
# - get_BMI.ipynb
# - get_Diagnoses.ipynb
# - get_Visit_Vars.ipynb
# - get_Antidiabetic_Drug_Exposure.ipynb
# - get_Antidiabetic_Timelines.ipynb
# - get_Diagnosis_Timeline.ipynb

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("participant_survey_answers_collapsed_prep.RData", verbose = T)
participant_survey_answers_collapsed_prep %>% head %>% print_all_cols

# %%
load("ehr_length_noObs_v2.rds")
ehr_length_tab <- ehr_length
#ehr_length_tab %>% head

# %%
load("participant_zip_state_region.RData")
place_data <- zip_data_merge_fmt %>% 
mutate(across(c(state,state_abbr,FIPS_state_numeric_code,region,region_code,division,division_code), ~replace(., zip_obs == "639**", NA))) %>%
mutate(across(c(state,state_abbr,FIPS_state_numeric_code,region,region_code,division,division_code), ~droplevels(.)))
remove(zip_data_merge_fmt)
place_data %>% filter(zip_obs == "639**") %>% head
#place_data %>% head

# %%
load("recent_bmi.rds")
recent_bmi <- recent_bmi
#recent_bmi %>% head

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
    full_join(place_data, by = "person_id") %>%
    full_join(recent_bmi, by = "person_id") %>%
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
filter(!is.na(age.survey) & !is.na(sex) & !is.na(race.ethnicity) & !is.na(Education.Level.bool) & !is.na(employed)) %>%
filter(!sex == "Other") %>% 
mutate(sex = droplevels(sex))

# %%
"\n\nTotal participants before applying eligibility requirements: " %>% cat
all.data.1 %>% nrow %>% comma %>% cat
"\n\nTotal participants with no EHR data: " %>% cat
all.data.1 %>% filter(!person_id %in% ehr_length_tab$person_id) %>% nrow %>% comma %>% cat
"\n\nTotal participants missing age, sex, or race/ethnicity data: " %>% cat
all.data.1 %>% filter(is.na(age.survey) | is.na(sex) | is.na(race.ethnicity)| is.na(Education.Level.bool)| is.na(employed)) %>% nrow %>% comma %>% cat
"\n\nTotal participants under 18: " %>% cat
all.data.1 %>% filter(Age2023 < 18) %>% nrow %>% comma %>% cat
"\n\nTotal participants who marked sex as 'other': " %>% cat
all.data.1 %>% filter(sex == "Other") %>% nrow %>% comma %>% cat
"\n\nTotal participants after applying requirements: " %>% cat
all.data.2 %>% nrow %>% comma %>% cat

# %%
"\n\nTotal participants before applying diagnosis eligibility requirements: " %>% cat
all.data.2 %>% nrow %>% comma %>% cat
"\n\nTotal participants with T2DM: " %>% cat
all.data.2 %>% filter(T2DM) %>% nrow %>% comma %>% cat
"\n\nTotal participants with MDD, no BD, no SCH: " %>% cat
all.data.2 %>% filter(MDD_noBDnoSCH) %>% nrow %>% comma %>% cat
"\n\nTotal participants with Obesity, Hypertension, Hypercholesterolemia, Hyperlipidemia, Heart Disease, Stroke, Chronic Kidney Disease, or A1C>8.5: " %>% cat
all.data.2 %>% filter(Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5) %>% nrow %>% comma %>% cat
"\n\nTotal participants with Pancreatitis, T1DM, Thyroid Cancer, or Gastroparesis: " %>% cat
all.data.2 %>% filter(Pancreatitis | T1DM | Thyroid_Cancer | Gastroparesis) %>% nrow %>% comma %>% cat
"\n\nTotal participants meeting all Wang Criteria for Eligibility: " %>% cat
all.data.2 %>% filter(Meets_Wang_Criteria) %>% nrow %>% comma %>% cat

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

drug_info

# %%
# Print summary
for(drug in drug_info$drug){
    use_column <- "{drug}_Use" %>% str_glue
    "\n\nTotal participants with {drug} Exposure and MDD diagnosis: " %>% str_glue %>% cat
    all.data.2 %>% filter(!!sym(use_column) & MDD_noBDnoSCH) %>% nrow %>% comma %>% cat
    "\n" %>% cat
}

# %%
# Print results
print_comp_cohort_info <- function(comparison_drug, this_criteria){
    var_name_semaglutide_pop <- paste0("Semaglutide_Population_for_Semaglutide_vs_", comparison_drug)
    var_name_other_drug_pop <- paste0(comparison_drug, "_Population_for_Semaglutide_vs_", comparison_drug)
    
    "Semaglutide vs {comparison_drug}" %>% str_glue %>% cat
    "\nSemaglutide group: " %>% cat
    this_semaglutide_group <- all.data.2 %>% 
    filter(!!sym(var_name_semaglutide_pop)) %>%
    filter(eval(parse(text = this_criteria)))
    
    this_semaglutide_group %>% nrow %>% comma %>% cat

    "\n\n{comparison_drug} group: " %>% str_glue %>% cat
    this_comparison_group <- all.data.2 %>% 
    filter(!!sym(var_name_other_drug_pop)) %>%
    filter(eval(parse(text = this_criteria)))
    
    this_comparison_group %>% nrow %>% comma %>% cat
    
    "\n\n" %>%cat
}

this_print_data <- c(
"MDD_noBDnoSCH", "MDD no BD/SCH participants meeting index timeline criteria (sufficient ehr, no 18-m window overlap)",
"MDD_noBDnoSCH & Meets_Wang_Criteria", "MDD no BD/SCH participants meeting index timeline criteria (sufficient ehr, no 18-m window overlap) and meeting all Wang Criteria for Eligibility",
"MDD_noBDnoSCH & Meets_Custom_Eligibility_Requirements", "MDD no BD/SCH participants meeting index timeline criteria (sufficient ehr, no 18-m window overlap) and meeting all custom eligibility requirements"
) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("this_criteria", "title"))

for(i in 1:nrow(this_print_data)){
    paste0(this_print_data$title[i], "\n\n") %>% cat
    for(j in 1:nrow(drug_info)){
        if(drug_info$drug[j] == "Semaglutide"){next}
        print_comp_cohort_info(drug_info$drug[j], this_print_data$this_criteria[i])
    }
}

# %%
cohort_names <- do.call(c, lapply(
  drug_info %>% filter(drug != "Semaglutide") %>% pull(drug),
  function(x) {
    c(paste0("Semaglutide_Population_for_Semaglutide_vs_", x), 
      paste0(x, "_Population_for_Semaglutide_vs_", x))
  }
))

test_criteria <- c("MDD & !BD & !SCH",
                   "MDD & !BD & !SCH & (T2DM | Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5)",
                   "MDD & !BD & !SCH & (T2DM | Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5) & !Pancreatitis & !Thyroid_Cancer & !Gastroparesis",
                   "MDD & !BD & !SCH & (T2DM | Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5) & !Pancreatitis & !Thyroid_Cancer & !Gastroparesis & !T1DM",
                   "MDD & !BD & !SCH & T2DM & (Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5) & !Pancreatitis & !Thyroid_Cancer & !Gastroparesis & !T1DM"
                  )
cohorts_table <- do.call(bind_rows, 
                         lapply(test_criteria,
                                function(x){
                                    all.data.2 %>%
                                    filter(eval(parse(text = x))) %>%
                                    summarise(across(all_of(cohort_names), ~sum(.))) %>%
                                    mutate(across(everything(), ~comma(.))) %>%
                                    mutate(filter = x) %>%
                                    relocate(filter, .before = 1)
                                }
                               )
                        )

colnames(cohorts_table) <- c("Eligibility Criteria", cohort_names %>% str_extract("^[^_]+_[^_]+") %>% str_replace_all("_", " "))

html_text <- cohorts_table %>%
kbl(escape = F, align = "c") %>%
kable_paper("hover", full_width = F) %>%
add_header_above(
    c(" " = 1, cohort_names %>% str_extract("[^_]+_[^_]+_[^_]+$") %>% str_replace_all("_", " ") %>% factor(levels = unique(.)) %>% table() %>% c)
) %>%
as.character 

html_text %>%
display_html

html_filename <- "html_tables/DTE_Cohort_Counts.html"
png_filename <- "DTE_Cohort_Counts.png"
system(paste0("rm ", html_filename))
writeLines(html_text, html_filename)
#webshot::webshot(html_filename, png_filename)

# %%
cohort_criteria <- "MDD & !BD & !SCH & (T2DM | Obese | Hypertension | Hypercholesterolemia | Hyperlipidemia | Heart_Disease | Stroke | Chronic_Kidney_Disease | A1C_over_8p5)"
all.data.3 <- all.data.2 %>% 
filter(eval(parse(text = cohort_criteria)))

# %%
this.data <- all.data.3

# %%
this.data %>% dim
this.data %>% head %>% print_all_cols

# %%
save(this.data, file = "dte_cohort_data.rds")
