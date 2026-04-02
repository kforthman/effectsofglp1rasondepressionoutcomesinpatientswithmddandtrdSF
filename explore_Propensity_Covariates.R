source("helper_functions.R")
this.data <- nontreat_result$dte_cohort_data2
all_groups

var_info <- read.csv("Data/ps_covariates.csv")  %>% 
  left_join(read.csv("Data/var_name_to_pretty.csv"), 
            by = join_by("var" == "var_name")) %>%
  arrange(pretty_var_category) %>% 
  filter(!var %in% c("Type_2_Diabetes_Mellitus_with_Complications", "Bariatric_Surgery", "Tobacco_Use_Disorder"))

# %% #MUST ADD Type_2_Diabetes_Mellitus_with_Complications and Bariatric_Surgery once EB adds them!!!
varsToFactor <- var_info %>% pull(var)

new_titles <- c("n", 
                var_info %>% 
                  mutate(pretty_var_category = ifelse(pretty_var_category == lag(pretty_var_category, default = ""), "", pretty_var_category)) %>%
                  pull(pretty_var_category)
)

new_names <- c("", 
               var_info %>% pull(pretty_name)
)


for(this_drug in all_groups){
  
  if(this_drug == "Semaglutide"){next}
  # display_markdown(paste("# Semaglutide vs ", this_drug))
  
  Semaglutide_Population_for_Semaglutide_vs_Drug <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
  Drug_Population_for_Semaglutide_vs_Drug <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)
  
  varname_semaglutide_age_at_index_years <- "Semaglutide_age_at_index_years"
  varname_thisdrug_age_at_index_years <- paste0(this_drug, "_age_at_index_years")
  
  varname_semaglutide_mdd_to_index_days <- "Semaglutide_mdd_to_index_days"
  varname_thisdrug_mdd_to_index_days <- paste0(this_drug, "_mdd_to_index_days")
  
  varname_semaglutide_index <- "Semaglutide_Index"
  varname_thisdrug_index <- paste0(this_drug, "_Index")
  
  this.data.subset <- this.data %>% 
    filter(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) | !!sym(Drug_Population_for_Semaglutide_vs_Drug)) %>%
    mutate(treatment = ifelse(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) == TRUE, 1, 0)) %>%
    mutate(age_at_index_years = ifelse(treatment, 
                                       !!sym(varname_semaglutide_age_at_index_years), 
                                       !!sym(varname_thisdrug_age_at_index_years))) %>%
    mutate(mdd_to_index_days = ifelse(treatment,
                                      !!sym(varname_semaglutide_mdd_to_index_days), 
                                      !!sym(varname_thisdrug_mdd_to_index_days)),
           mdd_to_index_years = time_length(ddays(mdd_to_index_days), "years")) %>%
    mutate(index = as.Date(ifelse(treatment,
                                  !!sym(varname_semaglutide_index), 
                                  !!sym(varname_thisdrug_index))),
           index_year = year(index)
    )
  
  my_table1(this.data = this.data.subset, 
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
