# Add diagnosis flags to all.data

source("helper_functions.R")

eligibility_inclusion_diagnoses_pretty <- data.frame(var_name = eligibility_inclusion_diagnoses) %>% 
  left_join(var_name_to_pretty, by = "var_name") %>% 
  pull(pretty_name)
all_drugs_pretty <- data.frame(var_name = all_drugs) %>% 
  left_join(var_name_to_pretty, by = "var_name") %>% 
  pull(pretty_name)


# %%
cohort_names <- do.call(c, lapply(
  comparator_drugs,
  function(x) {
    c(paste0(target_drug, "_Population_for_", target_drug,"_vs_", x), 
      paste0(x, "_Population_for_", target_drug,"_vs_", x))
  }
))

diagnosis_timeline_data %>% colnames

# %%
all.data <- nontreat_result$dte_cohort_data2 %>% 
  left_join(diagnosis_timeline_data, by = "PatientDurableKey")  %>%
  mutate(across(paste0(c(all_drugs, nontreatment_group), "_Index"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_Index$', '_Use', .col)}")) %>%
  filter(eval(parse(text = paste(cohort_names, collapse = " | "))))  %>%
  left_join(diag_table %>% 
              pivot_wider(names_from = Diagnosis, values_from = FirstDiagnosisDate, values_fill = NA, names_glue = "{Diagnosis}_Index")) %>%
  mutate(across(paste0(unique(diag_table$Diagnosis), "_Index"),
                ~ if_else(is.na(.), FALSE, TRUE),
                .names = "{sub('_Index$', '', .col)}"))

# %%
varsToFactor <- c(
  eligibility_inclusion_diagnoses, 
  paste0(eligibility_inclusion_diagnoses, "_Before_Drug_Index"),
  paste0(all_drugs, "_Use"),
  paste0(all_drugs, "_Overlaps_Drug_Index"))

new_titles <- c("n", 
                "Diagnoses", 
                rep("", length(eligibility_inclusion_diagnoses) - 1),
                "Diagnosis occurs before the index", 
                rep("", length(eligibility_inclusion_diagnoses) - 1),
                "Lifetime Antidiabetic Use", 
                rep("", length(all_drugs) - 1), 
                "Co-Occuring Antidiabetic Use within 6 months before or 12 months after index", 
                rep("", length(all_drugs) - 1)
)
new_names <- c("", 
               eligibility_inclusion_diagnoses_pretty,
               eligibility_inclusion_diagnoses_pretty,
               all_drugs_pretty,
               all_drugs_pretty
               )

for(this_drug in all_drugs){
  
  if(this_drug == target_drug){next}
  #display_markdown(paste("# ", target_drug," vs ", this_drug))
  
  Target_Population_for_Target_vs_Drug <- paste0(target_drug, "_Population_for_", target_drug,"_vs_", this_drug)
  Drug_Population_for_Target_vs_Drug <- paste0(this_drug, "_Population_for_", target_drug,"_vs_", this_drug)
  
  this.data <- all.data %>% 
    filter(!!sym(Target_Population_for_Target_vs_Drug) | !!sym(Drug_Population_for_Target_vs_Drug))
  
  for(this_diag in eligibility_inclusion_diagnoses){
    new_col <- paste0(this_diag, "_Before_Drug_Index")
    target_col <- paste0(this_diag, "_Before_", target_drug, "_Index")
    other_col <- paste0(this_diag, "_Before_", this_drug, "_Index")
    
    this.data <-  this.data %>% 
      mutate(!!new_col := ifelse(!!sym(Target_Population_for_Target_vs_Drug), !!sym(target_col), !!sym(other_col)))
  }
  
  for(this_demo_drug in all_drugs){
    if(this_demo_drug == target_drug){
      target_col <- paste0(target_drug, "_Use")
      other_col <- paste0(this_demo_drug, "_Overlaps_", this_drug, "_Index")
    }else if(this_demo_drug == this_drug){
      target_col <- paste0(this_demo_drug, "_Overlaps_", target_drug, "_Index")
      other_col <- paste0(this_demo_drug, "_Use")
    }else{
      target_col <- paste0(this_demo_drug, "_Overlaps_", target_drug, "_Index")
      other_col <- paste0(this_demo_drug, "_Overlaps_", this_drug, "_Index")
    }
    
    new_col <- paste0(this_demo_drug, "_Overlaps_Drug_Index")
    
    this.data <-  this.data %>% 
      mutate(!!new_col := ifelse(!!sym(Target_Population_for_Target_vs_Drug), !!sym(target_col), !!sym(other_col)))
  }
  
  my_table1(this.data = this.data, 
            my_strata = Drug_Population_for_Target_vs_Drug, 
            filename = paste0(target_drug, "_vs_", this_drug, "-Eligibility_Criteria"), 
            varsToFactor = varsToFactor,   
            new_colnames = c("Variable", 
                             "", 
                             paste0("TOTAL ", target_drug, " vs ", this_drug, " Population"), 
                             paste0(target_drug, " Population"), 
                             paste0(this_drug, " Population"), 
                             "p"),
            verbose = FALSE,
            new_titles = new_titles,
            new_names = new_names)
}

