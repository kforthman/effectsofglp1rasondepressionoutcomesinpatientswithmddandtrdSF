# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %% [markdown]
# # Load Data

# %%
participant_survey_answers_collapse <- read.csv("participant_survey_answers_collapsed.csv")

participant_survey_answers_collapse <- participant_survey_answers_collapse %>%
dplyr::select(person_id, 
              Health.Insurance..Insurance.Type.Update..Insurance.Type.Update..VA, 
              Active.Duty..Active.Duty.Serve.Status_Numeric) %>%
rename(VA_health_insurance = "Health.Insurance..Insurance.Type.Update..Insurance.Type.Update..VA",
       veteran = "Active.Duty..Active.Duty.Serve.Status_Numeric")

# %%
# Demographics table

load("participant_survey_answers_collapsed_prep.RData") #, verbose = T)

participant_survey_answers_collapsed_prep <- participant_survey_answers_collapsed_prep %>%
mutate(dummy = 1L, sex2 = sex) %>%
pivot_wider(
    names_from   = sex2,
    values_from  = dummy,
    values_fill  = list(dummy = 0L),
    names_prefix = "sex_"
) %>%
mutate(dummy = 1L, race.ethnicity2 = race.ethnicity) %>%
pivot_wider(
    names_from   = race.ethnicity2,
    values_from  = dummy,
    values_fill  = list(dummy = 0L),
    names_prefix = "race.ethnicity_",
    
) %>%
rename(race.ethnicity_Hispanic_or_Latino = `race.ethnicity_Hispanic or Latino`)
participant_survey_answers_collapsed_prep %>% colnames
#participant_survey_answers_collapsed_prep %>% head %>% print_all_cols

# %%
#EHR info table

load("ehr_length_noObs_v2.rds")
ehr_length_tab <- ehr_length
ehr_length_tab %>% colnames
#ehr_length_tab %>% head

# %% [markdown]
# Participants are defined as having Substance Use Disorder (SUD) if they have a record of one of the following diagnoses:
# - Alcohol Abuse or Dependence
# - Cannabis Abuse or Dependence
# - Cocaine Abuse or Dependence
# - Opioid Abuse or Dependence
# - Sedative Abuse or Dependence

# %%
# Substance use IDs

sud_ids <- rbind(
    read.csv("Data_Prepped/IDs-Alcohol_Abuse-Combined.txt", header = F),
    read.csv("Data_Prepped/IDs-Alcohol_Dependence-Combined.txt", header = F),
    read.csv("Data_Prepped/IDs-Cannabis_Abuse-Condition_Occurrence.txt", header = F),
    read.csv("Data_Prepped/IDs-Cannabis_Dependence-Condition_Occurrence.txt", header = F),
    read.csv("Data_Prepped/IDs-Cocaine_Abuse-Condition_Occurrence.txt", header = F),
    read.csv("Data_Prepped/IDs-Cocaine_Dependence-Condition_Occurrence.txt", header = F),
    read.csv("Data_Prepped/IDs-Opioid_Abuse-Condition_Occurrence.txt", header = F),
    read.csv("Data_Prepped/IDs-Opioid_Dependence-Condition_Occurrence.txt", header = F),
    read.csv("Data_Prepped/IDs-Sedative_Abuse-Condition_Occurrence.txt", header = F),
    read.csv("Data_Prepped/IDs-Sedative_Dependence-Condition_Occurrence.txt", header = F)
) %>%
pull(V1) %>%
unique

tud_ids <- read.csv("Data_Prepped/IDs-Tobacco_Use_Disorder-Combined.txt", header = F) %>%
pull(V1)

# %%
# Depression table

load("Data_Prepped/Prepped_Data-All_Participants-Depression-Condition_Occurrence.rds")
mdd_record <- data_prep

mdd_first_record <- mdd_record %>% 
group_by(person_id) %>% 
filter(start_datetime == min(start_datetime)) %>% 
dplyr::select(person_id, start_datetime) %>%
rename(first_depression_dx_date = start_datetime) %>%
slice(1) %>%
ungroup()

mdd_timeline <- mdd_first_record
remove(mdd_record, mdd_first_record)

#mdd_timeline %>% head

# %%
# Psych events table

load("Data_Prepped/Prepped_Data-All_Participants-Psychologic_or_Psychiatric_Visits_v2-Combined.rds") #, verbose = T)

psych_events <- data_prep

#psych_events %>% dim
#psych_events %>% head

# %%
# Grab the cohort

this_cohort <- mdd_timeline %>%
left_join(participant_survey_answers_collapsed_prep, by = "person_id") %>%
filter(!is.na(sex), !sex =="Other" & !is.na(race.ethnicity) & person_id %in% ehr_length$person_id & !is.na(age.survey)) %>%
pull(person_id)

# %%
# Grab the psych events for this cohort
this_cohort_psych_events_post_mdd <- psych_events %>%
filter(person_id %in% this_cohort) %>%
left_join(participant_survey_answers_collapsed_prep, by = "person_id") %>%
left_join(mdd_timeline, by = "person_id") %>%
filter(start_datetime >= first_depression_dx_date)  %>%
mutate(sex = droplevels(sex), race.ethnicity = droplevels(race.ethnicity)) %>%
mutate(substance_use_disorder = person_id %in% sud_ids,
       tobacco_use_disorder = person_id %in% tud_ids) 

# %%
# Derive total psych days

n_psych_days <- this_cohort_psych_events_post_mdd %>%
group_by(person_id, start_datetime) %>%
slice_head(n = 1) %>%
group_by(person_id) %>%
summarize(outcome_var = n(), .groups = "drop")

#n_psych_days %>% head

# %%
# Combine tables into final dataset

this_data <- mdd_timeline %>%
filter(person_id %in% this_cohort) %>%
left_join(participant_survey_answers_collapsed_prep,
          by = "person_id") %>%
left_join(participant_survey_answers_collapse,
          by = "person_id") %>%
left_join(ehr_length_tab %>%
          rename(ehr_start_date = "first_visit", ehr_end_date = "last_visit") %>%
          dplyr::select(person_id, ehr_start_date, ehr_end_date, DOB, Age2023, ehr_length), 
          by = "person_id") %>%
left_join(n_psych_days %>%
          rename(n_psych_days = "outcome_var"), 
          by = "person_id") %>%
filter(person_id %in% this_cohort) %>%
mutate(sex = droplevels(sex), race.ethnicity = droplevels(race.ethnicity)) %>%
mutate(substance_use_disorder = person_id %in% sud_ids,
       tobacco_use_disorder = person_id %in% tud_ids) %>%
mutate(n_psych_days = ifelse(is.na(n_psych_days), 
                             0, 
                             n_psych_days)) %>%
mutate(mdd_to_ehr_end_length_years = ifelse(is.na(first_depression_dx_date), 
                           0,
                           ifelse(is.na(ehr_end_date), 
                                  1/365.24219,
                                  ifelse(first_depression_dx_date == ehr_end_date,
                                        1/365.24219,
                                        difftime(as_date(ehr_end_date), as_date(first_depression_dx_date), units = "days")/365.24219
                                        )
                                 )
                           )) %>%
mutate(n_psych_days_per_year = ifelse(mdd_to_ehr_end_length_years>0, 
                                      floor(n_psych_days/mdd_to_ehr_end_length_years), 
                                      0)) %>%
mutate(age_at_mdd_dx = floor(time_length(interval(DOB, first_depression_dx_date), "years")))

#this_data %>% head %>% print_all_cols
this_data %>% colnames

# %% [markdown]
# # Results

# %% [markdown]
# The selected group is all individuals with MDD. Individuals have been excluded if
# - Sex is "Other" or missing
# - Race is missing
#
# Psych events are procedures with the following CPT codes:
# 90785, 90791, 90792, 90801, 90802, 90804-90819, 90821-90824, 90826, 90828, 90829, 90832-90834, 90836-90840, 90845-90847, 90849, 90853, 90857, 90862, 90863, 90865, 90867-90870, 90875, 90876, 90880, 90882, 90885, 90887, 90889, 90899
#
# Psych events are limited to events that occur after MDD diagnosis.
#
# Some in the cohort may not have any psych events. They are still included in all population counts

# %% [markdown]
# ### Cohort sex breakdown

# %%
total_count_sex <- participant_survey_answers_collapsed_prep %>%
filter(person_id %in% this_cohort) %>%
count(sex) %>%
pivot_wider(names_from = "sex", values_from = "n")

total_count_sex %>% 
mutate(Female = comma(Female), Male = comma(Male))

"The group is " %>% cat
percent(total_count_sex$Female / sum(total_count_sex)) %>% cat
" female." %>% cat

# %% [markdown]
# ### Cohort sex by SUD breakdown

# %%
total_count_sex_sud <- participant_survey_answers_collapsed_prep %>%
filter(person_id %in% this_cohort) %>%
mutate(substance_use_disorder = ifelse(person_id %in% sud_ids, "SUD", "not_SUD"),
       tobacco_use_disorder = person_id %in% tud_ids) %>%
count(sex, substance_use_disorder) %>%
pivot_wider(names_from = c("sex", "substance_use_disorder"), values_from = "n")

total_count_sex_sud %>% 
mutate(Female_SUD = comma(Female_SUD), Male_SUD = comma(Male_SUD),
       Female_not_SUD = comma(Female_not_SUD), Male_not_SUD = comma(Male_not_SUD))

percent(total_count_sex_sud$Female_SUD / (total_count_sex_sud$Female_SUD + total_count_sex_sud$Female_not_SUD)) %>% cat
" of the female population has SUD\n" %>% cat

percent(total_count_sex_sud$Male_SUD / (total_count_sex_sud$Male_SUD + total_count_sex_sud$Male_not_SUD)) %>% cat
" of the male population has SUD" %>% cat

# %% [markdown]
# ### Cohort by sex and existance of psych events

# %%
out <- participant_survey_answers_collapsed_prep %>%
filter(person_id %in% this_cohort) %>%
mutate(has_psych_event_post_mdd = person_id %in% this_cohort_psych_events_post_mdd$person_id) %>%
count(sex, has_psych_event_post_mdd) %>%
pivot_wider(names_from = "sex", values_from = "n") %>%
mutate(pct_Female = percent(Female/sum(Female)), 
       pct_Male = percent(Male/sum(Male)), 
       Female = comma(Female), 
       Male = comma(Male)) %>%
dplyr::select(has_psych_event_post_mdd, Female, pct_Female, Male, pct_Male)
out

out[[2,3]] %>% cat
" of females have at least one psych event after MDD diagnosis, while " %>% cat
out[[2,5]] %>% cat
" of males have at least one psych event after diagnosis" %>% cat

# %% [markdown]
# ### Psych event CPT codes by sex
#
# Arranged by magnitude of difference between male and female

# %%
this_cohort_psych_events_post_mdd %>%
count(sex, source_concept_code) %>%
pivot_wider(names_from = "sex", values_from = "n", values_fill = 0) %>%
mutate(n_per_Female = round(Female / total_count_sex$Female, 2),
       n_per_Male = round(Male / total_count_sex$Male, 2),
       Female = ifelse(Female < 20, "<20", comma(Female)),
       Male = ifelse(Male < 20, "<20", comma(Male)),
      dif = n_per_Male-n_per_Female) %>%
arrange(-abs(dif)) %>%
dplyr::select(source_concept_code, Female, Male)

# %% [markdown]
# The largest difference is for the CPT code 90853, which is code for "Group psychotherapy (other than of a multiple-family group)".

# %% [markdown]
# ### Psych event CPT codes by sex and SUD status

# %%
this_cohort_psych_events_post_mdd %>%
mutate(substance_use_disorder = ifelse(substance_use_disorder, "SUD", "not_SUD" )) %>%
count(sex, substance_use_disorder, source_concept_code) %>%
pivot_wider(names_from = c("sex", "substance_use_disorder"), values_from = "n", values_fill = 0) %>%
mutate(
    Male_SUD_orig = Male_SUD,
    Female_SUD = ifelse(Female_SUD < 20, "<20", comma(Female_SUD)),
    Female_not_SUD = ifelse(Female_not_SUD < 20, "<20", comma(Female_not_SUD)),
    Male_SUD = ifelse(Male_SUD < 20, "<20", comma(Male_SUD)),
    Male_not_SUD = ifelse(Male_not_SUD < 20, "<20", comma(Male_not_SUD)),
) %>%
arrange(-abs(Male_SUD_orig)) %>%
dplyr::select(-Male_SUD_orig)

# %% [markdown]
# ### Average count per year by sex and SUD status

# %%
psych_code_counts <- this_cohort_psych_events_post_mdd  %>%
left_join(this_data %>% dplyr::select(person_id, mdd_to_ehr_end_length_years), by = "person_id") %>%
count(person_id, source_concept_code, mdd_to_ehr_end_length_years) %>%
mutate(avg_n_per_year = n / mdd_to_ehr_end_length_years) %>%
dplyr::select(-n, -mdd_to_ehr_end_length_years) %>%
pivot_wider(names_from = "source_concept_code", values_from = "avg_n_per_year", values_fill = 0) %>%
left_join(participant_survey_answers_collapsed_prep %>% dplyr::select(person_id, sex)) %>%
mutate(SUD = person_id %in% sud_ids) %>%
mutate(sex = droplevels(sex))

psych_code_counts %>% colnames
# psych_code_counts %>% head

# %%
tableOne <- CreateTableOne(vars = c("90792", "90832", "90834", "90847", "90853", "90889", "90899", "90806", "90801", "90837", "90838", "90791", "90805", 
                                    "90857", "90862", "90808", "90833", "90804", "90807", "90816", "90836", "90840", "90875", "90870", "90885", "90802", 
                                    "90785", "90817", "90818", "90882", "90826", "90809", "90887", "90839", "90845", "90863", "90811", "90813", "90846", 
                                    "90880", "90812", "90876", "90819", "90849", "90810", "90823", "90828", "90867", "90868", "90869", "90821", "90815", 
                                    "90814", "90822", "90865", "90829"), 
                           strata = "sex",
                           data = psych_code_counts, 
                           addOverall = T)

tab3Mat <- print(tableOne, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab3Mat <- as.data.frame(tab3Mat) %>%
mutate(p_num = ifelse(row_number() == 1, 
                      -1, 
                      ifelse(p == "<0.001", 
                             0.0009, 
                             as.numeric(p)
                            )
                     )
      )%>% 
arrange(p_num) %>%
dplyr::select(-p_num)

tab3Mat

# %%
tableOne <- CreateTableOne(vars = c("90792", "90832", "90834", "90847", "90853", "90889", "90899", "90806", "90801", "90837", "90838", "90791", "90805", 
                                    "90857", "90862", "90808", "90833", "90804", "90807", "90816", "90836", "90840", "90875", "90870", "90885", "90802", 
                                    "90785", "90817", "90818", "90882", "90826", "90809", "90887", "90839", "90845", "90863", "90811", "90813", "90846", 
                                    "90880", "90812", "90876", "90819", "90849", "90810", "90823", "90828", "90867", "90868", "90869", "90821", "90815", 
                                    "90814", "90822", "90865", "90829"), 
                           strata = c("sex", "SUD"),
                           data = psych_code_counts, 
                           addOverall = T)

tab3Mat <- print(tableOne, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab3Mat <- as.data.frame(tab3Mat) %>%
mutate(p_num = ifelse(row_number() == 1, 
                      -1, 
                      ifelse(p == "<0.001", 
                             0.0009, 
                             as.numeric(p)
                            )
                     )
      )%>% 
arrange(p_num) %>%
dplyr::select(-p_num)

tab3Mat

# %% [markdown]
# ### Table1 breakdown by age, post MDD EHR length, and full EHR length

# %%
tableOne <- CreateTableOne(vars = c("n_psych_days_per_year", "mdd_to_ehr_end_length_years", "ehr_length", "age_at_mdd_dx", "Age2023",
                                    "substance_use_disorder", "tobacco_use_disorder"), strata = "sex",
                           data = this_data, 
                           addOverall = T)

tab3Mat <- print(tableOne, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab3Mat <- as.data.frame(tab3Mat)

tab3Mat

# %%
tableOne <- CreateTableOne(vars = c("n_psych_days_per_year", "mdd_to_ehr_end_length_years", "ehr_length", "age_at_mdd_dx", "Age2023",
                                    "substance_use_disorder", "tobacco_use_disorder"), strata = c("sex", "substance_use_disorder"),
                           data = this_data, 
                           addOverall = T)

tab3Mat <- print(tableOne, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab3Mat <- as.data.frame(tab3Mat)

tab3Mat

# %% [markdown]
# # Regression

# %%
m1 <- glm.nb(n_psych_days_per_year ~ race.ethnicity_White + sex_Male + Age2023 + substance_use_disorder + mdd_to_ehr_end_length_years,
             data = this_data)

summary(m1) %>% print
check_overdispersion(m1) %>% print
check_collinearity(m1) %>% print

tab <- tab_model(m1)
display_html(tab$knitr)

# %%
options(repr.plot.width=37, repr.plot.height=12)

ggplot(this_data, aes(x = n_psych_days_per_year, color = sex, fill = sex)) +
geom_density(alpha = 0.5)+
labs(
    title    = "Sex vs Psych Visit Frequency",
    x        = "Avg Psych Days Per Year",
    y        = "Count",
) +
  scale_x_continuous(trans = pseudo_log_trans(base = 10)) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10)) +
theme_minimal(base_size = 40)


# %%
this_data %>% count(is.na(n_psych_days_per_year))

# %%
options(repr.plot.width=37, repr.plot.height=12)

ggplot(this_data, aes(x = n_psych_days_per_year, color = sex, fill = sex)) +
geom_histogram(bins = 30, position = "dodge", alpha = 0.5)+
labs(
    title    = "Sex vs Psych Visit Frequency",
    x        = "Avg Psych Days Per Year",
    y        = "Count",
) +
  scale_x_continuous(trans = pseudo_log_trans(base = 10),
                    breaks = c(0, 1, 3, 10, 30, 100, 200, 300, 400, 500)
                    ) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10),
                    breaks = c(1, 10, 100, 1000, 10000, 100000)) +
theme_minimal(base_size = 40)


# %%
