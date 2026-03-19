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
source("my_table1.txt")
notebook_setup()

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
number_to_viridis <- function(x) {
  x <- as.numeric(x)
  # Ensure input is numeric
  if (x < 0 | x > 100) {
    stop("Input must be between 0 and 100.")
  }

  # Normalize non-NA values to [0, 1]
  normalized <- x / 100
  
  # Create a palette of 256 colors on the viridis scale
  palette <- viridis(256)
  
  # Map normalized values to indices between 1 and 256
  indices <- round(normalized * 225) + 1
  
  # Assign the corresponding colors
  palette[indices]
}

my_table1 <- function(this.data, my_strata, filename, varsToFactor, new_names = NULL, new_titles = NULL, new_colnames = NULL, verbose = FALSE){

    csv_filename <- paste0(filename, ".csv")
    frmt_filename <- paste0(filename, "-frmt.rds")
    html_filename <- paste0("html_tables/", filename, ".html")
    png_filename <- paste0(filename, ".png")
    
    tableOne <- CreateTableOne(vars = varsToFactor, strata = my_strata, 
                               data = this.data, 
                            addOverall = T)
    tab3Mat <- print(tableOne, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
    tab3Mat <- as.data.frame(tab3Mat)
    

    tab3Mat <- tab3Mat %>% mutate(p = replace(p, p == NaN, ""),
                                  p = replace(p, p == "NA", ""),
                                  p_num = ifelse(p == "<0.001", 0, p))

    tab3Mat <- tab3Mat %>% mutate(p_num = as.numeric(p_num))

    if(verbose){
        cat("Original row names: \"")
        cat(paste(rownames(tab3Mat), collapse = "\", \""))
        cat("\"")
    }
    
    tab3Mat$nn <- rownames(tab3Mat)
    tab3Mat$title <- rownames(tab3Mat)
    row.names(tab3Mat) <- 1:nrow(tab3Mat)
    colnames(tab3Mat) <- c("TOTAL", "minus", "plus","p","test", "p_num", "n2", "n1")
    
    frmt_dataset <- tab3Mat
    # Format table for readability
    
    if(!is.null(new_names)){tab3Mat$nn <- new_names}
    if(!is.null(new_titles)){tab3Mat$title <- new_titles}
    if(!is.null(new_colnames)){colnames(tab3Mat) <- new_colnames}
    
    write.csv(tab3Mat, csv_filename, row.names = F)
    
    # Format for HTML
    max.pval <- 0.2 # Maximum p-value (for color scale)
    for(i in 1:nrow(frmt_dataset)){
        if(is.na(frmt_dataset$p_num[i])){frmt_dataset$p[i] <- "";next}
        if(frmt_dataset$p_num[i] < 0.05){ # if entry is statistically significant
          frmt_dataset$p[i] <- cell_spec(
            format(frmt_dataset$p[i], nsmall = 6), 
            bold = T, 
            color = "black",
            background = spec_color(frmt_dataset$p_num[i], 
                                    begin = 0, end = 0.9, option = "magma", direction = -1, 
                                    scale_from = c(0,max.pval))#,
          )
      }else{ # if entry is not significant
        frmt_dataset$p[i] <- cell_spec(
            format(frmt_dataset$p[i], nsmall = 6), 
            bold = T, 
            color = "black"
          )
      }
    }
    
    
        frmt_dataset <- frmt_dataset %>%
    mutate(across(all_of(c("TOTAL", "minus", "plus")), 
                  ~ifelse(grepl("mean\\.\\.SD", n2), as.numeric(gsub("^([0-9.]+).*|.*", "\\1", .)), comma(as.numeric(gsub("^$|^(\\d+).*|.*", "\\1", .)))), 
                  .names = "{.col}.freq")) %>%
    mutate(across(all_of(c("TOTAL", "minus", "plus")), 
                  ~gsub("^$|.*\\(([0-9.]+)\\).*|.*", "\\1", .), 
                  .names = "{.col}.rate")) %>%
    rowwise() %>%
    mutate(across(all_of(c("TOTAL.rate", "minus.rate", "plus.rate")), 
                  ~ifelse(. == "" | grepl("mean\\.\\.SD", n2) | is.na(as.numeric(.)), 
                          ., 
                          cell_spec(paste0(.,"%"), color = "white", background = number_to_viridis(.))
                         ),
                  .names = "{.col}")) %>%
    ungroup() %>%
    dplyr::select(n1,n2,TOTAL.freq, TOTAL.rate,minus.freq,minus.rate, plus.freq, plus.rate, p) %>% 
    replace(is.na(.), "")
    
    if(!is.null(new_names)){frmt_dataset$n2 <- new_names}
    if(!is.null(new_titles)){frmt_dataset$n1 <- new_titles}
    if(!is.null(new_colnames)){colnames(frmt_dataset) <- c(new_colnames[1:3], "", new_colnames[4], "", new_colnames[5], "", new_colnames[6])}
    
    save(frmt_dataset, file = frmt_filename)
    html_text <- kbl(frmt_dataset, escape = F, align = "r") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 16)%>%
      scroll_box(width = "100%", height = "1000px")

    html_text <- as.character(html_text)
    IRdisplay::display_html(
    html_text
        )

    system(paste0("rm ", html_filename))
    writeLines(html_text, html_filename)
    #webshot::webshot(html_filename, png_filename)
    }

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
