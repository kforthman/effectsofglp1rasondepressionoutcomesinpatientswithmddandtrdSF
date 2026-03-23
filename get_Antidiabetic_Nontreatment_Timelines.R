# %% [markdown]
# # get: Antidiabetic Nontreatment Timelines
#
# This script defines periods of time for which participants do not experience a change in antidiabetic treatment. Only use of a new drug counts as a change. Reccurrent use of a drug after discontinuation does not count as a change. Eligible time is masked before 6 months before MDD first diagnosis and after beginning Semaglutide.

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %% [markdown]
# ## Load Data

# %%
# ehr length
load("ehr_length_noObs_v2.rds")

ehr_length %>% head

# %%
# depression record
load("Data_Prepped/Prepped_Data-All_Participants-Depression-Condition_Occurrence.rds")
mdd_record <- data_prep

# %%
# antidiabetic drug records
drug_timeline_info <- c("Semaglutide", "Data_Prepped/Prepped_Data-All_Participants-Semaglutide-Drug_Exposure.rds",
                        "Insulins", "Data_Prepped/Prepped_Data-All_Participants-Insulins-Drug_Exposure.rds",
                        "Metformin", "Data_Prepped/Prepped_Data-All_Participants-Metformin-Drug_Exposure.rds",
                        "DPP4i", "Data_Prepped/Prepped_Data-All_Participants-DPP_4i-Drug_Exposure.rds",
                        "SGLT2i", "Data_Prepped/Prepped_Data-All_Participants-SGLT2i-Drug_Exposure.rds",
                        "SU", "Data_Prepped/Prepped_Data-All_Participants-SU-Drug_Exposure.rds",
                        "TZD", "Data_Prepped/Prepped_Data-All_Participants-TZD-Drug_Exposure.rds",
                        "GLP1RA", "Data_Prepped/Prepped_Data-All_Participants-GLP_1RAs_Excluding_Semaglutide-Drug_Exposure.rds"
                        ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug", "file_path"))

# Loop over each file path
for(i in 1:nrow(drug_timeline_info)){
    paste0("Loading file \"", drug_timeline_info$file_path[i], "\"\n") %>% cat
    load(drug_timeline_info$file_path[i])
    
    var_name <- paste0(drug_timeline_info$drug[i], "_data")
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, data_prep)
}

# %% [markdown]
# ## Create Antidiabetic Non-treatment timeline

# %%
# first depression diagnosis
mdd_first_record <- mdd_record %>% 
group_by(person_id) %>% 
filter(start_datetime == min(start_datetime)) %>% 
dplyr::select(person_id, start_datetime) %>%
rename(first_depression_dx_date = start_datetime) %>%
slice(1) %>%
ungroup() %>%
mutate(mo6_before_first_depression_dx_date = first_depression_dx_date %m-% months(6))

mdd_first_record %>% head

# %%
# first semaglutide record
semaglutide_first_record <- Semaglutide_data %>%
arrange(person_id, start_datetime) %>%
group_by(person_id) %>%
slice(1) %>%
rename(first_semaglutide_date = "start_datetime") %>%
dplyr::select(person_id, first_semaglutide_date)

semaglutide_first_record %>% head

# %%
# all antidiabetic records
antidiabetic_timeline <- rbind(
    Semaglutide_data,
    Insulins_data,
    Metformin_data,
    DPP4i_data,
    SGLT2i_data,
    SU_data,
    TZD_data,
    GLP1RA_data
) %>%
arrange(person_id, start_datetime) %>%
mutate(concept_name = replace(concept_name, concept_name == "GLP-1RAs excluding Semaglutide", "GLP-1RA")) %>%
# filter to the first use of each drug type
group_by(person_id, concept_name) %>% 
slice(1) %>%
ungroup()%>%
mutate(concept_name = paste0(concept_name, " first record")) %>%
# filter to events that happen 6 months before MDD diagnosis or after
left_join(mdd_first_record, by = "person_id") %>% 
filter(!is.na(first_depression_dx_date)) %>%
filter(start_datetime >= mo6_before_first_depression_dx_date)%>%
# filter to events that happen before Semaglutide prescription
left_join(semaglutide_first_record, by = "person_id") %>% 
filter(is.na(first_semaglutide_date) | 
       start_datetime < first_semaglutide_date)%>%
# Formatting
rename(date = "start_datetime") %>%
dplyr::select(person_id, date, concept_name)

antidiabetic_timeline %>% head

# %%
ehr_timeline <- ehr_length %>%
# Ensure that all participants have a final visit. If last visit is missing, it is set as equal to the first visit date.
mutate(last_visit = as.Date(ifelse(is.na(last_visit), first_visit, last_visit))) %>%
# Formatting
pivot_longer(cols = c(first_visit, last_visit), names_to = "concept_name", values_to = "date") %>% 
mutate(concept_name = replace(concept_name, concept_name == "first_visit", "EHR begins"),
       concept_name = replace(concept_name, concept_name == "last_visit", "EHR ends")) %>%
dplyr::select(person_id, date, concept_name)

ehr_timeline %>% head

# %%
mdd_timeline <- mdd_first_record %>% 
rename(date = "mo6_before_first_depression_dx_date") %>% 
mutate(concept_name = "6 months before MDD first diagnosis") %>%
dplyr::select(-first_depression_dx_date)

mdd_timeline %>% head

# %%
semaglutide_timeline <- semaglutide_first_record %>%
rename(date = "first_semaglutide_date") %>%
mutate(concept_name = "Semaglutide first record")

semaglutide_timeline %>% head

# %%
treatment_timeline <- rbind(antidiabetic_timeline,
                               ehr_timeline,
                               mdd_timeline,
                               semaglutide_timeline) %>%
# Filter to individuals with an MDD first diagnosis date.
filter(person_id %in% unique(mdd_first_record$person_id)) %>%
mutate(concept_name = factor(concept_name, 
                             levels = c("EHR begins",
                                        "6 months before MDD first diagnosis",
                                        "DPP-4i first record", 
                                        "Insulins first record",
                                        "Metformin first record",
                                        "SU first record",
                                        "SGLT-2i first record",
                                        "GLP-1RA first record",
                                        "TZD first record",
                                        "Semaglutide first record",
                                        "EHR ends"))) %>%
arrange(person_id, date, concept_name) %>%
# Only 6mo before MDD or EHR begin can be the first record. Must remove one if both exist.
mutate(first_record = ifelse(concept_name %in% c("EHR begins", "6 months before MDD first diagnosis"),
                             "First eligible record",
                             concept_name)) %>%
group_by(person_id, first_record) %>%
slice_tail(n = 1) %>%
ungroup() %>%
# Only Semaglutide or EHR end can be the final record. Must remove one if both exist.
mutate(final_record = ifelse(concept_name %in% c("Semaglutide first record", "EHR ends"),
                             "Final eligible record",
                             concept_name)) %>%
group_by(person_id, final_record) %>%
slice(1) %>%
ungroup() %>%
arrange(person_id, date, concept_name) %>%
dplyr::select(-final_record, -first_record)

treatment_timeline %>% filter(person_id %in% c(1000000, 1000222, 1001290)) # these participants do not have MDD #, 1000186, 1001485, 1001602

# %%
nonswitch_periods <- treatment_timeline %>% 
# Create periods between timeline events.
mutate(
    start_date = as.Date(ifelse(concept_name %in% c("EHR begins", "6 months before MDD first diagnosis"), date, date + 1)),
    end_date = as.Date(ifelse("EHR ends" == lead(concept_name), lead(date), lead(date) - 1)),
    concept_name = paste0(concept_name, " to ", lead(concept_name))
) %>%
group_by(person_id) %>%
filter(row_number() != n()) %>% # The last row is removed because it includes the data of two participants
ungroup() %>%
dplyr::select(person_id, concept_name, start_date, end_date) %>%
# Filter to eligible periods (at least 18 months long).
mutate(window_length = time_length(interval(start_date, end_date), "months")) %>%
filter(window_length >= 18)  %>%
# Add time from MDD during period to the dataset.
left_join(mdd_first_record %>% dplyr::select(person_id, first_depression_dx_date)) %>%
filter(!is.na(first_depression_dx_date)) %>%
mutate(
    at_6_months_after_start_date = start_date %m+% months(6),
    at_12_months_before_end_date = end_date %m-% months(12),
    tfe_at_index_bgn = floor(time_length(interval(first_depression_dx_date, at_6_months_after_start_date), "days")),
    tfe_at_index_end = floor(time_length(interval(first_depression_dx_date, at_12_months_before_end_date), "days")),
)

nonswitch_periods %>% head

# %%
nonswitch_periods %>% pull(person_id) %>% unique %>% length %>% comma

# %%
this.data <- nonswitch_periods
save(this.data, file = "data_DTE_AntidiabeticNontreatmentTimeline.rds")

# %% [markdown]
# ## Identify the eligible treatment and comparison population

# %%
load("dte_cohort_data.rds")
dte_cohort_data <- this.data

# %%
sema_data <- dte_cohort_data %>%
filter(Semaglutide_Meets_Timeline_Criteria == 1)

# %%
# tfe is time from eligibility
sema_tfe_dist <- sema_data %>% 
count(Semaglutide_mdd_to_index_days)%>%
mutate(freq = n / sum(n))

sema_tfe_dist %>% head

# %%
options(repr.plot.width=15, repr.plot.height=10)

ggplot(sema_tfe_dist, aes(x = Semaglutide_mdd_to_index_days, y = n)) +
geom_bar(stat = "identity", fill = "salmon") + theme_minimal(base_size = 40)

# %%
nonswitch_periods_eligible <- nonswitch_periods %>%
# Meets inclusion criteria
filter(person_id %in% dte_cohort_data$person_id) %>%
# Remove those already assigned to the treatment group
filter(!person_id %in% sema_data$person_id)

# %%
n_comp <- nonswitch_periods_eligible %>%
pull(person_id) %>%
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
    person_id = unique(nonswitch_periods_eligible$person_id),

    emu_tfe_at_index_days = sample(
        x       = sema_tfe_dist$Semaglutide_mdd_to_index_days,
        size    = n_comp,
        replace = TRUE,
        prob    = sema_tfe_dist$freq   # frequencies or weights
    )
)

nonswitch_periods_selected <- nonswitch_periods_eligible %>%
left_join(tfe_at_index, by = join_by(person_id == person_id)) %>%
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
geom_bar(stat = "identity") +
theme_minimal(base_size = 40)

s_samp <- dist_comp %>% filter(group == "Semaglutide") %>% dplyr::select(-group) %>% uncount(weights = n) %>% pull(tfe_at_index_days)
c_samp <- dist_comp %>% filter(group == "Comparison") %>% dplyr::select(-group) %>% uncount(weights = n) %>% pull(tfe_at_index_days)

ks.test(s_samp, c_samp)

# %% [markdown]
# ## Finalize

# %%
nonswitch_periods_selected %>% head

# %%
nonswitch_periods_selected2 <- nonswitch_periods_selected %>%
mutate(Nontreatment_index = first_depression_dx_date + emu_tfe_at_index_days) %>%
rename(Nontreatment_mdd_to_index_days = "emu_tfe_at_index_days")

nonswitch_periods_selected2 %>% head

# %%
# formatted for storage of all comparisons
dte_cohort_data2 <- dte_cohort_data %>%
mutate(Semaglutide_Population_for_Semaglutide_vs_Nontreatment = person_id %in% sema_data$person_id,
       Nontreatment_Population_for_Semaglutide_vs_Nontreatment   = person_id %in% nonswitch_periods_selected$person_id) %>%
left_join(nonswitch_periods_selected2 %>% dplyr::select(person_id, Nontreatment_mdd_to_index_days, Nontreatment_index), by = join_by(person_id == person_id)) %>%
mutate(Nontreatment_age_at_index_years = floor(time_length(interval(DOB, Nontreatment_index), "years")))

# %%
# formatted for comparison of the 2 groups
dte_cohort_data3 <- dte_cohort_data2 %>%
left_join(mdd_first_record, by = join_by(person_id == person_id)) %>%
filter(Semaglutide_Population_for_Semaglutide_vs_Nontreatment | Nontreatment_Population_for_Semaglutide_vs_Nontreatment) %>%
mutate(treatment = ifelse(Semaglutide_Population_for_Semaglutide_vs_Nontreatment, 1, 0)) %>%
mutate(treatment_name = ifelse(treatment, "Semaglutide", "Nontreatment")) %>%
mutate(index_date = as.Date(ifelse(treatment, Semaglutide_first_drug_record, Nontreatment_index))) %>%
mutate(index_year = year(index_date)) %>%
mutate(time_diag_to_index_days = ifelse(treatment, Semaglutide_mdd_to_index_days, Nontreatment_mdd_to_index_days)) %>%
mutate(age_at_index_years = ifelse(treatment, Semaglutide_age_at_index_years, Nontreatment_age_at_index_years))

# %%
ggplot(dte_cohort_data3, aes(x = age_at_index_years)) +
geom_histogram(aes(y=..density.., color = treatment_name, fill = treatment_name), alpha=0.4, position = "dodge") +
geom_density(aes(color = treatment_name)) +
theme_minimal(base_size = 40)

s_samp <- dte_cohort_data3 %>% filter(treatment_name == "Semaglutide")  %>% pull(age_at_index_years)
c_samp <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(age_at_index_years)

ks.test(s_samp, c_samp)

# %%
ggplot(dte_cohort_data3, aes(x = time_diag_to_index_days)) +
geom_histogram(aes(y=..density.., color = treatment_name, fill = treatment_name), alpha=0.4, position = "dodge") +
geom_density(aes(color = treatment_name)) +
theme_minimal(base_size = 40)

s_samp <- dte_cohort_data3 %>% filter(treatment_name == "Semaglutide")  %>% pull(time_diag_to_index_days)
c_samp <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(time_diag_to_index_days)

ks.test(s_samp, c_samp)

# %%
ggplot(dte_cohort_data3, aes(x = index_year)) +
geom_histogram(aes(y=..density.., color = treatment_name, fill = treatment_name), alpha=0.4, position = "dodge") +
geom_density(aes(color = treatment_name)) +
theme_minimal(base_size = 40)

s_samp <- dte_cohort_data3 %>% filter(treatment_name == "Semaglutide")  %>% pull(index_year)
c_samp <- dte_cohort_data3 %>% filter(treatment_name == "Nontreatment") %>% pull(index_year)

ks.test(s_samp, c_samp)

# %%
length(unique(nonswitch_periods_selected$person_id)) == nrow(nonswitch_periods_selected)

# %%
this.data <- dte_cohort_data2

save(this.data, file = "dte_cohort_wNontreat_data.rds")

# %%
