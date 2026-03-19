# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
matchingVars<- read.csv("ps_covariates.csv") %>% mutate(issues = NA)
matchingVars

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

drug_info

# %%
for(i in 1:nrow(drug_info)){
    this_drug <- drug_info$drug[i]
    if(this_drug == "Semaglutide"){next}
    this_ps_cov_list <- read.csv(paste0("PS_Covariates-", this_drug, ".csv"),row.names = 1)
    matchingVars <- matchingVars %>% mutate(!!sym(this_drug) := var %in% this_ps_cov_list$var)
}

# %%
PS_Covariates_summary <- matchingVars %>% 
rowwise() %>%
  mutate(
    sum = sum(c_across(Insulins:Nontreatment))
  ) %>% 
arrange(-sum, Insulins, Metformin, DPP4i, SGLT2i, SU, TZD, GLP1RA, Nontreatment) %>%
dplyr::select(-sum, -var_type, - issues)

# %%
PS_Covariates_summary

# %%
write.csv(PS_Covariates_summary, "PS_Covariates-Summary.csv")

# %%
PS_Covariates_summary %>%
filter(Insulins) %>%
pull(var) %>%
paste(collapse = " + ")

# %%
PS_Covariates_summary %>%
pull(var) %>%
paste(collapse = " + ")
