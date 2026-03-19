# %% [markdown]
# pre-requisites:
# - get_DTE_cohort.ipynb

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

# %%
hist(dte_cohort_data$Average_Total_All_Visits_PerYear)
median(dte_cohort_data$Average_Total_All_Visits_PerYear)

# %%
hist(dte_cohort_data$Average_Total_Outpatient_Visits_PerYear)
median(dte_cohort_data$Average_Total_Outpatient_Visits_PerYear)

# %%
hist(dte_cohort_data$Average_Total_Psychologic_or_Psychiatric_Procedure_or_Service_PerYear)
median(dte_cohort_data$Average_Total_Psychologic_or_Psychiatric_Procedure_or_Service_PerYear)

# %%
