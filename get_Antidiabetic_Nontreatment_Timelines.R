# %% [markdown]
# # get: Antidiabetic Nontreatment Timelines
#
# This script defines periods of time for which participants do not experience a change in antidiabetic treatment. Only use of a new drug counts as a change. Reccurrent use of a drug after discontinuation does not count as a change. Eligible time is masked before 6 months before MDD first diagnosis and after beginning Semaglutide.

library(readr)
library(tidyverse)
library(scales)



# %% [markdown]
# ## Identify the eligible treatment and comparison population
mdd_data <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260318-1500/MDDPatientList_Table-26_03_18-v1.csv",
                     na = c("", "NA", "NULL", "null"), 
                     col_types = cols(
                       PatientDurableKey      = col_character(),
                       MDD_Index              = col_datetime(format = "%Y-%m-%d"),
                       Mo6_Before_MDD_Index   = col_datetime(format = "%Y-%m-%d"),
                       Eligibility_Group_A    = col_logical(),
                       Eligibility_Group_B    = col_logical(),
                       Eligibility_Group_C    = col_logical(),
                       Eligibility_Group_D    = col_logical(),
                       Eligibility_Group_E    = col_logical(),
                       First_Patient_Record   = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                       Last_Patient_Record    = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                       HasInclA               = col_logical(),
                       HasOrInclA             = col_logical(),
                       HasInclB               = col_logical(),
                       HasOrInclB             = col_logical(),
                       HasExclA               = col_logical(),
                       HasExclB               = col_logical(),
                       HasExclC               = col_logical(),
                       BirthDate              = col_datetime(format = "%Y-%m-%d")
                     ))%>%
  mutate(First_Patient_Record  = as.Date(First_Patient_Record),
         Last_Patient_Record   = as.Date(Last_Patient_Record))

# %%
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
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, Eligibility_Group_B), by = "PatientDurableKey") %>%
  filter(Eligibility_Group_B)




nonswitch_periods <- read_csv("/Volumes/Studies/ehr_study/uploaded-data/20260318-1500/Nontreatment_Table-26_03_18-v1.csv", col_types = cols(
  PatientDurableKey       = col_character(),
  StartConcept            = col_character(),
  StartDate               = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  EndConcept              = col_character(),
  EndDate                 = col_datetime(format = "%Y-%m-%d %H:%M:%S")
)) %>%
  mutate(StartDate = as.Date(StartDate),
         EndDate   = as.Date(EndDate))  %>% 
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, Eligibility_Group_B), by = "PatientDurableKey") %>%
  filter(Eligibility_Group_B) %>%
  mutate(
    at_6_months_after_start_date = StartDate + days(180),
    at_12_months_before_end_date = EndDate - days(365),
    tfe_at_index_bgn = floor(time_length(interval(MDD_Index, at_6_months_after_start_date), "days")),
    tfe_at_index_end = floor(time_length(interval(MDD_Index, at_12_months_before_end_date), "days")),
  )

# test
# nonswitch_periods %>% # before filter
#   left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, Eligibility_Group_B), by = "PatientDurableKey") %>%
#   filter(!Eligibility_Group_B) %>%
#   select(PatientDurableKey) %>%
#   write.csv("Data/Nontreatment-Ineligible.csv", row.names = F)

# test
# dte_cohort_data %>% # before filter
#   left_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index, Eligibility_Group_B), by = "PatientDurableKey") %>%
#   filter(Semaglutide_vs_Insulins_AllCriteriaMet & !Eligibility_Group_B) %>%
#   select(PatientDurableKey) %>%
#   write.csv("Data/Semaglutide_vs_Insulins-Ineligible.csv", row.names = F)

# %%
sema_data <- dte_cohort_data %>%
  filter(Semaglutide_PrePostInclusionCriteriaMet == 1 & MDD_before_Semaglutide == 1) %>%
  mutate(Semaglutide_mdd_to_index_days = as.numeric(difftime(Semaglutide_Index, MDD_Index, units = "days")))

# %%
# tfe is time from eligibility
sema_tfe_dist <- sema_data %>% 
  count(Semaglutide_mdd_to_index_days)%>%
  mutate(freq = n / sum(n))

# %%
# options(repr.plot.width=15, repr.plot.height=10)
# 
ggplot(sema_tfe_dist, aes(x = Semaglutide_mdd_to_index_days, y = n)) +
  geom_bar(stat = "identity", fill = "salmon")

# %%
nonswitch_periods_eligible <- nonswitch_periods %>%
  # Remove those already assigned to the treatment group
  filter(!PatientDurableKey %in% sema_data$PatientDurableKey)

# %%
n_comp <- nonswitch_periods_eligible %>%
  pull(PatientDurableKey) %>%
  unique %>%
  length

n_sema <- sema_data %>% nrow

match_ratio <- floor(n_comp/n_sema)

cat(sprintf(
  "There are %s individuals eligible for the non-treatment population and %s in the treatment population. Match ratio of %d to 1.",
  comma(n_comp),
  comma(n_sema),
  match_ratio
)
)

# %% [markdown]
# ## Index date emulation

# %% [markdown]
# ###  Method 2: Index Date Emulation
# Randomly sample from the age distribution of the treated sample to assign age at index in the untreated sample. Throw out untreated patients whose emulated index is outside of an eligible period.

# %%
set.seed(2025)
tfe_at_index <- data.frame(
  PatientDurableKey = unique(nonswitch_periods_eligible$PatientDurableKey),
  
  emu_tfe_at_index_days = sample(
    x       = sema_tfe_dist$Semaglutide_mdd_to_index_days,
    size    = n_comp,
    replace = TRUE,
    prob    = sema_tfe_dist$freq   # frequencies or weights
  )
)

nonswitch_periods_selected <- nonswitch_periods_eligible %>%
  left_join(tfe_at_index, by = "PatientDurableKey") %>%
  filter(tfe_at_index_bgn <= emu_tfe_at_index_days & tfe_at_index_end >= emu_tfe_at_index_days)

nonswitch_periods_selected %>% nrow %>% comma
nonswitch_periods_selected %>% head

# %%
dist_comp <- sema_tfe_dist %>%
  rename(Semaglutide = n, 
         tfe_at_index_days = Semaglutide_mdd_to_index_days) %>%
  dplyr::select(-freq) %>%
  left_join(
    nonswitch_periods_selected %>% 
      count(emu_tfe_at_index_days) %>%
      rename(Comparison = n, 
             tfe_at_index_days = emu_tfe_at_index_days), 
    by = join_by(tfe_at_index_days == tfe_at_index_days)) %>%
  pivot_longer(c(Semaglutide, Comparison), names_to = "group", values_to = "n", values_drop_na = TRUE)

ggplot(dist_comp, aes(x = tfe_at_index_days, y = n, fill = group)) +
  geom_bar(stat = "identity")

s_samp <- dist_comp %>% filter(group == "Semaglutide") %>% dplyr::select(-group) %>% uncount(weights = n) %>% pull(tfe_at_index_days)
c_samp <- dist_comp %>% filter(group == "Comparison") %>% dplyr::select(-group) %>% uncount(weights = n) %>% pull(tfe_at_index_days)

ks.test(s_samp, c_samp)

# %% [markdown]
# ## Finalize

# %%
nonswitch_periods_selected %>% head

# %%
nonswitch_periods_selected2 <- nonswitch_periods_selected %>%
  mutate(Nontreatment_Index = MDD_Index + days(emu_tfe_at_index_days)) %>%
  rename(Nontreatment_mdd_to_index_days = "emu_tfe_at_index_days")

nonswitch_periods_selected2 %>% head

# %% Semaglutide_vs_Insulins_AllCriteriaMet
# formatted for storage of all comparisons
dte_cohort_data2 <- dte_cohort_data %>%
  left_join(nonswitch_periods_selected2 %>% dplyr::select(PatientDurableKey, Nontreatment_Index), by = "PatientDurableKey") %>%
  mutate(Nontreatment_Iplus15                          = Nontreatment_Index + days(15),
         Nontreatment_Iplus365                         = Nontreatment_Index + days(365),
         MDD_before_Nontreatment                       = PatientDurableKey %in% nonswitch_periods_selected$PatientDurableKey,
         Nontreatment_PreIndexEncounterCount           = NA,
         Nontreatment_PostIndexEncounterCount          = NA,
         Nontreatment_PrePostInclusionCriteriaMet      = PatientDurableKey %in% nonswitch_periods_selected$PatientDurableKey,
         Semaglutide_vs_Nontreatment_AllCriteriaMet    = PatientDurableKey %in% sema_data$PatientDurableKey,
         Nontreatment_vs_Semaglutide_AllCriteriaMet    = PatientDurableKey %in% nonswitch_periods_selected$PatientDurableKey
         )

# %%
# formatted for comparison of the 2 groups
dte_cohort_data3 <- dte_cohort_data2 %>%
  left_join(mdd_data %>% dplyr::select(PatientDurableKey, BirthDate), by = "PatientDurableKey") %>%
  filter(Semaglutide_vs_Nontreatment_AllCriteriaMet | Nontreatment_vs_Semaglutide_AllCriteriaMet) %>%
  mutate(treatment = ifelse(Semaglutide_vs_Nontreatment_AllCriteriaMet, 1, 0)) %>%
  mutate(treatment_name = ifelse(treatment, "Semaglutide", "Nontreatment")) %>%
  mutate(index_date = as.Date(ifelse(treatment, Semaglutide_Index, Nontreatment_Index))) %>%
  mutate(index_year = year(index_date)) %>%
  mutate(Semaglutide_mdd_to_index_days = as.numeric(difftime(Semaglutide_Index, MDD_Index, units = "days"))) %>%
  mutate(Nontreatment_mdd_to_index_days = as.numeric(difftime(Nontreatment_Index, MDD_Index, units = "days"))) %>%
  mutate(Semaglutide_age_at_index_years = floor(time_length(interval(BirthDate, Semaglutide_Index), "years"))) %>%
  mutate(Nontreatment_age_at_index_years = floor(time_length(interval(BirthDate, Nontreatment_Index), "years"))) %>%
  mutate(time_diag_to_index_days = ifelse(treatment, Semaglutide_mdd_to_index_days, Nontreatment_mdd_to_index_days)) %>%
  mutate(age_at_index_years = ifelse(treatment, Semaglutide_age_at_index_years, Nontreatment_age_at_index_years))

# %%
ggplot(dte_cohort_data3, aes(x = age_at_index_years)) +
  geom_histogram(aes(y = after_stat(density), color = treatment_name, fill = treatment_name), alpha=0.4, position = "dodge") +
  geom_density(aes(color = treatment_name))

s_samp <- dte_cohort_data3 %>% filter(treatment_name == "Semaglutide")  %>% pull(age_at_index_years)
c_samp <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(age_at_index_years)

ks.test(s_samp, c_samp)

# %%
ggplot(dte_cohort_data3, aes(x = time_diag_to_index_days)) +
  geom_histogram(aes(y = after_stat(density), color = treatment_name, fill = treatment_name), alpha=0.4, position = "dodge") +
  geom_density(aes(color = treatment_name))

s_samp <- dte_cohort_data3 %>% filter(treatment_name == "Semaglutide")  %>% pull(time_diag_to_index_days)
c_samp <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(time_diag_to_index_days)

ks.test(s_samp, c_samp)

# %%
ggplot(dte_cohort_data3, aes(x = index_year)) +
  geom_histogram(aes(y = after_stat(density), color = treatment_name, fill = treatment_name), alpha=0.4, position = "dodge") +
  geom_density(aes(color = treatment_name))

s_samp <- dte_cohort_data3 %>% filter(treatment_name == "Semaglutide")  %>% pull(index_year)
c_samp <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(index_year)

ks.test(s_samp, c_samp)

# %%
length(unique(nonswitch_periods_selected$PatientDurableKey)) == nrow(nonswitch_periods_selected)

# %%
this.data <- dte_cohort_data2

save(this.data, file = "Data/dte_cohort_wNontreat_data.rds")

# %%
