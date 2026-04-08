this_ps_covariates <- ps_covariates
# %%
for(i in 1:length(comparator_groups)){
  this_drug <- comparator_groups[i]
  this_ps_cov_list <- read.csv(paste0("OutputData/PS_Covariates-", this_drug, ".csv"))
  this_ps_covariates <- this_ps_covariates %>% mutate(!!sym(this_drug) := var %in% this_ps_cov_list$var)
}

# %%
PS_Covariates_summary <- this_ps_covariates %>% 
  rowwise() %>%
  mutate(
    sum = rowSums(across(where(is.logical)), na.rm = TRUE)
  ) %>% 
  arrange(-sum, across(all_of(comparator_groups))) %>%
  dplyr::select(var, all_of(comparator_groups))

# %%
write.csv(PS_Covariates_summary, "OutputData/PS_Covariates-Summary.csv")
