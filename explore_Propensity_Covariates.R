# %% [markdown]
# pre-requisites:
# - get_DTE_cohort.ipynb

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()
source("helper_functions.R")
Sys.setenv(OPENSSL_CONF="/dev/null")

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

# %%
drug_info <- c("Semaglutide",
                        "Insulins", 
                        "Metformin", 
                        "DPP4i", 
                        "SGLT2i",
                        "SU", 
                        "TZD", 
                        "GLP1RA",
                        "Nontreatment"
                        ) %>%
matrix(ncol = 1, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug"))


# %%
varsToFactor <- c(
    "age.survey",
    "age_at_index_years",
    "mdd_to_index_years",
    "sex", 
    "race.ethnicity",
    "Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances",
    "Problems_Related_to_Lifestyle",

    "Type_2_Diabetes_Mellitus_with_Complications", 
    "Obese",
    "Bariatric_Surgery",

    "ADHD",
    "Agoraphobia", 
    "Anxiety_Disorder_NOS", 
    "Generalized_Anxiety", 
    "OCD",
    "Panic_Disorder", 
    "PTSD",
    "Social_Anxiety_Disorder",

    "Alcohol_Abuse",
    "Alcohol_Dependence",
    "Cannabis_Abuse", 
    "Cannabis_Dependence", 
    "Cocaine_Abuse",
    "Cocaine_Dependence",
    "Opioid_Abuse",
    "Opioid_Dependence",
    "Sedative_Abuse",
    "Sedative_Dependence", 
    "Tobacco_Use_Disorder", 

    "Hypertension", 
    "Hypercholesterolemia", 
    "Hyperlipidemia", 
    "Heart_Disease", 
    "Stroke", 
    "Diseases_of_the_Arteries_Artrioles_and_Capillaries",

    "Antidepressants",
    "Average_Total_All_Visits_PerYear", 
    "Average_Total_Outpatient_Visits_PerYear", 
    "Average_Total_Psychologic_or_Psychiatric_Procedure_or_Service_PerYear"
)

new_titles <- c("n", 
                "", 
                "",
                "",
                "Sex", 
                "Race/Ethinicity", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Lifestyle and SES", 
                "", 
                "Problems Related to Type 2 Diabetes", 
                "", 
                "", 
                "Psychiatric Comorbidities", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Substance Abuse and Dependence", 
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
                "Heart Related Disorders", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Health System Utilization", 
                "", 
                "", 
                "")

new_names <- c("", 
               "Age (mean (SD))", 
               "Age at Index (mean (SD))",
               "MDD to Index Years (mean (SD))",
               "Male", 
               "", 
               "Asian", 
               "Black", 
               "White", 
               "Hispanic or Latino", 
               "Other", 
               "Persons with Potential Health Hazards Related to Socioeconomic and Psychosocial Circumstances", 
               "Problems Related to Lifestyle", 
               "Type 2 Diabetes Mellitus with Complications", 
               "Obese", 
               "Bariatric Surgery", 
               "ADHD", 
               "Agoraphobia", 
               "Anxiety Disorder NOS", 
               "Generalized Anxiety", 
               "OCD", 
               "Panic Disorder", 
               "PTSD", 
               "Social Anxiety Disorder", 
               "Alcohol Abuse", 
               "Alcohol Dependence", 
               "Cannabis Abuse", 
               "Cannabis Dependence", 
               "Cocaine Abuse", 
               "Cocaine Dependence", 
               "Opioid Abuse", 
               "Opioid Dependence", 
               "Sedative Abuse", 
               "Sedative Dependence", 
               "Tobacco Use Disorder", 
               "Hypertension", 
               "Hypercholesterolemia", 
               "Hyperlipidemia", 
               "Heart Disease", 
               "Stroke", 
               "Diseases of the Arteries Artrioles and Capillaries", 
               "Antidepressants",
               "Average Total All Visits Per Year (mean (SD))", 
               "Average Total Outpatient Visits Per Year (mean (SD))", 
               "Average Total Psychologic or Psychiatric Procedure or Service Per Year (mean (SD))")


for(this_drug in drug_info$drug){

    if(this_drug == "Semaglutide"){next}
    display_markdown(paste("# Semaglutide vs ", this_drug))

    Semaglutide_Population_for_Semaglutide_vs_Drug <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    Drug_Population_for_Semaglutide_vs_Drug <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    varname_semaglutide_age_at_index_years <- "Semaglutide_age_at_index_years"
    varname_thisdrug_age_at_index_years <- paste0(this_drug, "_age_at_index_years")

    varname_semaglutide_mdd_to_index_days <- "Semaglutide_mdd_to_index_days"
    varname_thisdrug_mdd_to_index_days <- paste0(this_drug, "_mdd_to_index_days")

    this.data <- dte_cohort_data %>% 
    filter(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) | !!sym(Drug_Population_for_Semaglutide_vs_Drug)) %>%
    mutate(treatment = ifelse(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) == TRUE, 1, 0)) %>%
    mutate(age_at_index_years = ifelse(treatment, 
                                       !!sym(varname_semaglutide_age_at_index_years), 
                                       !!sym(varname_thisdrug_age_at_index_years))) %>%
    mutate(mdd_to_index_days = ifelse(treatment,
                                      !!sym(varname_semaglutide_mdd_to_index_days), 
                                      !!sym(varname_thisdrug_mdd_to_index_days)),
          mdd_to_index_years = time_length(ddays(mdd_to_index_days), "years"))

    my_table1(this.data = this.data, 
              my_strata = Drug_Population_for_Semaglutide_vs_Drug, 
              filename = paste0("Semaglutide_vs_", this_drug, "-Basic_Demographics"), 
              varsToFactor = varsToFactor,   
              new_colnames = c("Variable", "", paste0("TOTAL Semaglutide vs ", this_drug, " Population"), "Semaglutide Population", paste0(this_drug, " Population"), "p"),
              verbose = FALSE,
              new_titles = new_titles,
              new_names = new_names
              )
}

# %%

# %%
