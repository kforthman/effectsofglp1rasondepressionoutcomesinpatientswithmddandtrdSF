# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup(packages_to_install = c("bigrquery", "questionr", "tidyverse", "reshape2","scales",
                        "tableone", "tidyr", "dplyr", "stringr","RCurl",
                        "RJSONIO", "lubridate", "svMisc", "IRdisplay","plotly",
                        "RColorBrewer", "grDevices"))

# %%
print_as_c <- function(my_list){
    paste0("'", my_list %>% unique %>% sort %>% paste(collapse = "',\n'"), "'") %>% cat
}

# %%
participant_survey_answers_collapse <- read.csv("participant_survey_answers_collapsed.csv")

# %%
explore_data(participant_survey_answers_collapse)

# %%
all.data <- data.frame(person_id = participant_survey_answers_collapse$person_id)

# %% [markdown]
# ## Insurance

# %%
participant_survey_answers_collapse$Insurance..Health.Insurance_Numeric %>% print_as_c

# %%
all.data$insured <- NA
all.data$insured[participant_survey_answers_collapse$Insurance..Health.Insurance_Numeric == 1] <- TRUE
all.data$insured[participant_survey_answers_collapse$Insurance..Health.Insurance_Numeric == 0] <- FALSE

# %% [markdown]
# ## Seen a mental health professional

# %%
participant_survey_answers_collapse$Health.Advice..Spoken.To.Mental.Health.Professional_Numeric %>% print_as_c

# %%
all.data$mh_professional <- participant_survey_answers_collapse$Health.Advice..Spoken.To.Mental.Health.Professional_Numeric == 1

# %% [markdown]
# ## Marital Status

# %%
participant_survey_answers_collapse$'Marital.Status..Current.Marital.Status' %>% print_as_c

# %%
all.data <- all.data %>%
mutate(
    Marital.Status..Current.Marital.Status.simp = 
    participant_survey_answers_collapse$'Marital.Status..Current.Marital.Status'
) %>%
mutate(
    Marital.Status..Current.Marital.Status.simp =
    ifelse(Marital.Status..Current.Marital.Status.simp %in% c("PMI: Skip", "PMI: Prefer Not To Answer"), 
                                                              NA, 
                                                              Marital.Status..Current.Marital.Status.simp
          )
    )
all.data$'Marital.Status..Current.Marital.Status.simp' %>% table

# %% [markdown]
# ## Smoking, cig num

# %%
participant_survey_answers_collapse$'Smoking..Average.Daily.Cigarette.Number' %>% print_as_c

# %%
all.data <- all.data %>%
mutate(Smoking..Average.Daily.Cigarette.Number.simp = as.numeric(participant_survey_answers_collapse$'Smoking..Average.Daily.Cigarette.Number')) %>%
mutate(Smoking..Average.Daily.Cigarette.Number.simp = 
       ifelse(Smoking..Average.Daily.Cigarette.Number.simp %in% c("PMI: Skip", "PMI: Dont Know", 'PMI: Prefer Not To Answer'),
             NA,
             Smoking..Average.Daily.Cigarette.Number.simp))
all.data$'Smoking..Average.Daily.Cigarette.Number.simp' %>% hist

# %% [markdown]
# ## Employment

# %%
participant_survey_answers_collapse %>% select(starts_with("Employment..Employment.Status..Employment.Status")) %>% colnames() %>% print_as_c

# %%
all.data.emp <- participant_survey_answers_collapse[,c('Employment..Employment.Status..Employment.Status..Employed.For.Wages',
                                                       'Employment..Employment.Status..Employment.Status..Self.Employed',
                                                       'Employment..Employment.Status..Employment.Status..Out.Of.Work.One.Or.More',
                                                       'Employment..Employment.Status..Employment.Status..Out.Of.Work.Less.Than.One',
                                                       'Employment..Employment.Status..Employment.Status..Homemaker',
                                                       'Employment..Employment.Status..Employment.Status..Student',
                                                       'Employment..Employment.Status..Employment.Status..Retired',
                                                       'Employment..Employment.Status..Employment.Status..Unable.To.Work')]
colnames(all.data.emp) <- c('Employed.For.Wages',
                   'Self.Employed',
                   'Out.Of.Work.One.Or.More',
                   'Out.Of.Work.Less.Than.One',
                   'Homemaker',
                   'Student',
                   'Retired',
                   'Unable.To.Work')
all.data.emp$n_selected <- rowSums(all.data.emp, na.rm = T)
combine_true_columns <- function(row) {
  # Get the names of columns where the row value is TRUE
  true_columns <- names(row)[row == TRUE & !is.na(row)]
  
  # Combine the column names into a single string, separated by commas
  combined <- paste(true_columns, collapse = ", ")
  
  return(combined)
}
all.data.emp$emp_status <- all.data.emp %>% dplyr::select(!n_selected) %>% apply(1, combine_true_columns)

all.data.emp <- all.data.emp %>% mutate(emp_status_trump = 
                       ifelse(
                       Out.Of.Work.Less.Than.One,
                       "Out.Of.Work.Less.Than.One", NA)) %>% 
                mutate(emp_status_trump = 
                       ifelse(
                       Out.Of.Work.One.Or.More,
                       "Out.Of.Work.One.Or.More", emp_status_trump)) %>% 
                mutate(emp_status_trump = 
                       ifelse(
                       Unable.To.Work,
                       "Unable.To.Work", emp_status_trump)) %>% 
                mutate(emp_status_trump = 
                       ifelse(
                       Homemaker,
                       "Homemaker", emp_status_trump)) %>% 
                mutate(emp_status_trump = 
                       ifelse(
                       Student,
                       "Student", emp_status_trump)) %>% 
                mutate(emp_status_trump = 
                       ifelse(
                       Retired,
                       "Retired", emp_status_trump)) %>% 
                mutate(emp_status_trump = 
                       ifelse(
                       Self.Employed,
                       "Self.Employed", emp_status_trump)) %>% 
                mutate(emp_status_trump = 
                       ifelse(
                       Employed.For.Wages,
                       "Employed.For.Wages",emp_status_trump))

all.data.emp.summ <- all.data.emp %>% mutate(Freq = 1) %>% group_by(emp_status, emp_status_trump,n_selected) %>% summarise(Freq = sum(Freq))
all.data.emp.summ %>% write.csv("emp_status_summary.csv")

all.data.emp.summ

# %%
all.data$employment_status_trump <- all.data.emp$emp_status_trump

# %%
all.data <- all.data %>%
            mutate(employed = 
                   ifelse(
                   employment_status_trump %in% c('Out.Of.Work.One.Or.More',
                                                  'Out.Of.Work.Less.Than.One',
                                                  'Homemaker',
                                                  'Student',
                                                  'Retired',
                                                  'Unable.To.Work'),
                   "Not In Labor Force or Unemployed", NA)) %>%
            mutate(employed = 
                   ifelse(
                   employment_status_trump %in% c('Employed.For.Wages',
                                                  'Self.Employed'),
                   "Employed", employed)) 

all.data$employed <- all.data$employed %>% factor
all.data$employed %>% table

# %% [markdown]
# ## Smoking status

# %%
participant_survey_answers_collapse %>% select((contains("toba")|contains("smok")|contains("100"))) %>% colnames() %>% print_as_c

# %%
all.data$smoker <- (participant_survey_answers_collapse %>%
mutate(smoker = 
       !is.na(Smoking..100.Cigs.Lifetime_Numeric) &
       !is.na(Smoking..Current.Daily.Cigarette.Number) &
       as.logical(Smoking..100.Cigs.Lifetime_Numeric) &
       Smoking..Current.Daily.Cigarette.Number > 0
))$smoker

# %%
all.data$smoker %>% table / nrow(all.data)

# %%
colnames(all.data)

# %%
colnames(participant_survey_answers_collapse)[grepl("date", colnames(participant_survey_answers_collapse))]

# %% [markdown]
# ## Age

# %%
participant_survey_answers_collapse$first_survey_date <- as.Date(participant_survey_answers_collapse$first_survey_date, "%Y-%m-%d")
participant_survey_answers_collapse$birth_datetime <- as.Date(participant_survey_answers_collapse$birth_datetime, "%Y-%m-%d")

# %%
all.data$birth_datetime <- participant_survey_answers_collapse$birth_datetime

# %%
all.data$age.survey <- (interval(participant_survey_answers_collapse$birth_datetime,participant_survey_answers_collapse$first_survey_date) / dyears()) %>% round
all.data$age.survey[1:10]

# %%
# Define breakpoints for the bins
breakpoints <- c(-Inf, 24, 34, 44, 64, Inf)

# Define labels for the bins
labels <- c("18 - 24 years", "25 - 34 years", "35 - 44 years", "45 - 64 years", "65 years and over")

# Create a new variable with the categorical age ranges
all.data$age_group_5 <- cut(all.data$age.survey, breaks = breakpoints, labels = labels, right = TRUE, include.lowest = TRUE)
all.data$age_group_5 <- all.data$age_group_5 %>% factor
all.data$age_group_5[1:10]

# %%
# Define breakpoints for the bins
breakpoints <- c(-Inf, 34, 44, 64, Inf)

# Define labels for the bins
labels <- c("18 - 34 years", "35 - 44 years", "45 - 64 years", "65 years and over")

# Create a new variable with the categorical age ranges
all.data$age_group_4 <- cut(all.data$age.survey, breaks = breakpoints, labels = labels, right = TRUE, include.lowest = TRUE)
all.data$age_group_4 <- all.data$age_group_4 %>% factor
all.data$age_group_4[1:10]

# %%
#all.data$age.period.start <- (interval(all.data$birth_datetime,all.data$period.start) / dyears()) %>% round
#all.data$age.period.start[1:10]

# %% [markdown]
# ## Sex at birth

# %%
participant_survey_answers_collapse$sex_at_birth_source_value %>% print_as_c

# %%
all.data$sex <- participant_survey_answers_collapse$sex_at_birth_source_value
all.data$sex[all.data$sex %in% c("No matching concept",
                                 "SexAtBirth_Intersex",
                                 "SexAtBirth_SexAtBirthNoneOfThese")
                          ] <- "Other"
all.data$sex[all.data$sex %in% c('SexAtBirth_Male')
                      ] <- "Male"
all.data$sex[all.data$sex %in% c('SexAtBirth_Female')
                      ] <- "Female"
all.data$sex[all.data$sex %in% c("PMI_Skip", "PMI_PreferNotToAnswer")
                          ] <- NA
all.data$sex <- all.data$sex %>% factor(levels = c("Female", "Male", "Other"))
all.data$sex %>% table

# %% [markdown]
# ## Gender

# %%
participant_survey_answers_collapse$gender_source_value %>% print_as_c

# %%
all.data$gender <- participant_survey_answers_collapse$gender_source_value
all.data$gender[all.data$gender %in% c('GenderIdentity_AdditionalOptions', 
                                                'GenderIdentity_GeneralizedDiffGender', 
                                                'GenderIdentity_NonBinary', 
                                                'GenderIdentity_Transgender')
                      ] <- "Other"
all.data$gender[all.data$gender %in% c('PMI_PreferNotToAnswer', 
                                                'PMI_Skip')
                      ] <- NA
all.data$gender[all.data$gender %in% c('GenderIdentity_Man')
                      ] <- "Man"
all.data$gender[all.data$gender %in% c('GenderIdentity_Woman')
                      ] <- "Woman"
all.data$gender <- all.data$gender %>% factor(levels = c("Woman", "Man", "Other"))
all.data$gender %>% table

# %% [markdown]
# ## Disabled

# %%
participant_survey_answers_collapse %>% select(starts_with("Disability") & contains("Numeric")) %>% colnames %>% print_as_c

# %%
all.data$disabled <- (participant_survey_answers_collapse[,c('Disability..Blind_Numeric', 
            'Disability..Deaf_Numeric', 
            'Disability..Difficulty.Concentrating_Numeric', 
            'Disability..Dressing.Bathing_Numeric', 
            'Disability..Errands.Alone_Numeric', 
            'Disability..Walking.Climbing_Numeric')] %>% rowSums(na.rm = T)) > 0

# %% [markdown]
# ## Annual Income

# %%
participant_survey_answers_collapse$Income..Annual.Income %>% print_as_c

# %%
#as.factor(all.data$Income..Annual.Income) %>% levels %>% paste(collapse = "','") %>% cat
all.data$Income..Annual.Income.factor <- factor(participant_survey_answers_collapse$Income..Annual.Income, levels = c(
    'Annual Income: less 10k',
    'Annual Income: 10k 25k',
    'Annual Income: 25k 35k',
    'Annual Income: less 35k',
    'Annual Income: 35k 50k',
    'Annual Income: 50k 75k',
    'Annual Income: 35k 75k',
    'Annual Income: 75k 100k',
    'Annual Income: 100k 150k',
    'Annual Income: 75k 150k',
    'Annual Income: more 150k',
    'Annual Income: 150k 200k',
    'Annual Income: more 200k',
    'PMI: Prefer Not To Answer',
    'PMI: Skip'))
all.data$Income..Annual.Income.factor %>% table

# %%
all.data$Income..Annual.Income.factor[all.data$Income..Annual.Income.factor %in% c(
    'Annual Income: less 10k',
    'Annual Income: 10k 25k',
    'Annual Income: 25k 35k')] <- 'Annual Income: less 35k'
all.data$Income..Annual.Income.factor[all.data$Income..Annual.Income.factor %in% c(
    'Annual Income: 35k 50k',
    'Annual Income: 50k 75k')] <- 'Annual Income: 35k 75k'
all.data$Income..Annual.Income.factor[all.data$Income..Annual.Income.factor %in% c(
    'Annual Income: 75k 100k',
    'Annual Income: 100k 150k')] <- 'Annual Income: 75k 150k'
all.data$Income..Annual.Income.factor[all.data$Income..Annual.Income.factor %in% c(
    'Annual Income: 150k 200k',
    'Annual Income: more 200k')] <- 'Annual Income: more 150k'
all.data$Income..Annual.Income.factor <- all.data$Income..Annual.Income.factor %>% droplevels(c(
    'Annual Income: less 10k',
    'Annual Income: 10k 25k',
    'Annual Income: 25k 35k',
    'Annual Income: 35k 50k',
    'Annual Income: 50k 75k',
    'Annual Income: 75k 100k',
    'Annual Income: 100k 150k',
    'Annual Income: 150k 200k',
    'Annual Income: more 200k',
    'PMI: Prefer Not To Answer',
    'PMI: Skip',
    NA))
all.data$Income..Annual.Income.factor %>% table
((all.data$Income..Annual.Income.factor %>% table) / nrow(all.data)) %>% round(2)

# %% [markdown]
# ## Education Level

# %%
participant_survey_answers_collapse$Education.Level..Highest.Grade %>% print_as_c

# %%
#as.factor(all.data$Education.Level..Highest.Grade) %>% levels %>% paste(collapse = "','") %>% cat
all.data$Education.Level <- factor(participant_survey_answers_collapse$Education.Level..Highest.Grade, levels = c(
    'Highest Grade: Never Attended',
    'Highest Grade: One Through Four',
    'Highest Grade: Five Through Eight',
    'Highest Grade: Nine Through Eleven',
    'No HS Degree, No GED',
    'Highest Grade: Twelve Or GED',
    'Highest Grade: College One to Three',
    'Twelve Or GED',
    'Highest Grade: College Graduate',
    'Highest Grade: Advanced Degree',
    'College Or Advanced Degree',
    
    'PMI: Prefer Not To Answer',
    'PMI: Skip'))
all.data$Education.Level %>% table

# %%
all.data$Education.Level[all.data$Education.Level %in% c(
    'Highest Grade: Never Attended',
    'Highest Grade: One Through Four',
    'Highest Grade: Five Through Eight',
    'Highest Grade: Nine Through Eleven')] <- 'No HS Degree, No GED'
all.data$Education.Level[all.data$Education.Level %in% c(
    'Highest Grade: Twelve Or GED',
    'Highest Grade: College One to Three')] <- 'Twelve Or GED'
all.data$Education.Level[all.data$Education.Level %in% c(
    'Highest Grade: College Graduate',
    'Highest Grade: Advanced Degree')] <- 'College Or Advanced Degree'
all.data$Education.Level <- all.data$Education.Level %>% droplevels(c(
    'Highest Grade: Never Attended',
    'Highest Grade: One Through Four',
    'Highest Grade: Five Through Eight',
    'Highest Grade: Nine Through Eleven',
    'Highest Grade: Twelve Or GED',
    'Highest Grade: College One to Three',
    'Highest Grade: College Graduate',
    'Highest Grade: Advanced Degree',
    'PMI: Prefer Not To Answer',
    'PMI: Skip',NA))
all.data$Education.Level %>% levels
((all.data$Education.Level %>% table) / nrow(all.data)) %>% round(2)
all.data$Education.Level[1:20]
participant_survey_answers_collapse$Education.Level..Highest.Grade[1:20]

# %%
all.data$Education.Level.bool <- as.character(all.data$Education.Level)
all.data$Education.Level.bool[all.data$Education.Level.bool %in% c("Twelve Or GED", "No HS Degree, No GED")] <- "No Higher Degree"
all.data$Education.Level.bool <- all.data$Education.Level.bool %>% factor

# %% [markdown]
# ## Ethnicity

# %%
participant_survey_answers_collapse$ethnicity_source_value %>% print_as_c

# %%
all.data$ethnicity <- factor(participant_survey_answers_collapse$ethnicity_source_value, levels = c(
    'Hispanic',
    'Not Hispanic',
    'PMI_PreferNotToAnswer',
    'PMI_Skip',
    'WhatRaceEthnicity_RaceEthnicityNoneOfThese'
))
all.data$ethnicity %>% table

# %%
all.data$ethnicity[all.data$ethnicity %in% c(
    'Not Hispanic',
    'WhatRaceEthnicity_RaceEthnicityNoneOfThese')] <- 'Not Hispanic or Latino'
all.data$ethnicity <- all.data$ethnicity %>% droplevels(c(
    'PMI_PreferNotToAnswer',
    'PMI_Skip'))
levels(all.data$ethnicity)[1:2] <- c('Hispanic or Latino','Not Hispanic or Latino')
all.data$ethnicity %>% levels
all.data$ethnicity %>% table

# %% [markdown]
# ## Race

# %%
participant_survey_answers_collapse$race_source_value %>% print_as_c

# %%
all.data$race <- factor(participant_survey_answers_collapse$race_source_value, levels = c(
    'WhatRaceEthnicity_Asian',
    'WhatRaceEthnicity_Black',
    'WhatRaceEthnicity_White',
    'WhatRaceEthnicity_MENA', #middle east north africa
    'WhatRaceEthnicity_NHPI', #native hawaiian and pacific islander
    'WhatRaceEthnicity_GeneralizedMultPopulations',
    'WhatRaceEthnicity_RaceEthnicityNoneOfThese',
    'AoUDRC_NoneIndicated',
    'PMI_PreferNotToAnswer',
    'PMI_Skip'))
all.data$race %>% table

# %%
all.data$race[all.data$race %in% c(
    'WhatRaceEthnicity_MENA', #middle east north africa
    'WhatRaceEthnicity_NHPI', #native hawaiian and pacific islander
    'WhatRaceEthnicity_GeneralizedMultPopulations',
    'WhatRaceEthnicity_RaceEthnicityNoneOfThese')] <- 'Other'
all.data$race <- all.data$race %>% droplevels(c(
    'WhatRaceEthnicity_MENA', #middle east north africa
    'WhatRaceEthnicity_NHPI', #native hawaiian and pacific islander
    'WhatRaceEthnicity_GeneralizedMultPopulations',
    'WhatRaceEthnicity_RaceEthnicityNoneOfThese',
    'AoUDRC_NoneIndicated',
    'PMI_PreferNotToAnswer',
    'PMI_Skip'))
levels(all.data$race)[1:4] <- c('Asian','Black','White','Other')
all.data$race %>% levels
all.data$race %>% table

# %% [markdown]
# ## Marital Status

# %%
participant_survey_answers_collapse$Marital.Status..Current.Marital.Status_Numeric %>% print_as_c

# %%
all.data$married.num <- participant_survey_answers_collapse$Marital.Status..Current.Marital.Status_Numeric == 1
all.data$married <- ifelse(all.data$married.num, 
                                   "married", "not_married")
all.data$married <- all.data$married %>% factor

# %% [markdown]
# ## Race/Ethnicity

# %%
all.data$race.ethnicity <- all.data$race %>% as.character
all.data$race.ethnicity[all.data$ethnicity == "Hispanic or Latino"] <- "Hispanic or Latino"

all.data$white.only <- all.data$race.ethnicity
all.data$white.only[all.data$race.ethnicity %in% c("Asian", "Black", "Hispanic or Latino", "Other")] <- "Non White"

all.data$white.only <- all.data$white.only %>% factor
all.data$race.ethnicity <- factor(all.data$race.ethnicity, levels = c('Asian','Black','White','Hispanic or Latino','Other'))

# %%
all.data %>% head

# %% [markdown]
# ## Mental Health Professional

# %%
all.data$mh_professional <- participant_survey_answers_collapse$Health.Advice..Spoken.To.Mental.Health.Professional_Numeric == 1

# %% [markdown]
# ## Alcohol

# %%
all.data$Alcohol..Average.Daily.Drink.Count_Numeric <- participant_survey_answers_collapse$Alcohol..Average.Daily.Drink.Count_Numeric

# %% [markdown]
# ## Create some different formats for the variables

# %%
all.data <- all.data %>% 
mutate(dummy = 1L,
       sex1 = sex) %>%
pivot_wider(
    names_from   = sex1,
    values_from  = dummy,
    values_fill  = list(dummy = 0L),
    names_prefix = "sex_"
  ) %>%
mutate(dummy = 1L,
       race.ethnicity1 = race.ethnicity) %>%
pivot_wider(
    names_from   = race.ethnicity1,
    values_from  = dummy,
    values_fill  = list(dummy = 0L),
    names_prefix = "race.ethnicity_"
  ) %>%
rename(race.ethnicity_Hispanic_or_Latino = `race.ethnicity_Hispanic or Latino`) %>%
mutate(higher_education = Education.Level.bool == "College Or Advanced Degree") %>%
mutate(employed_num = employed == "Employed")

# %% [markdown]
# ## Write

# %%
all.data <- all.data %>% mutate(person_id = as.character(person_id))

# %%
write.csv(all.data, "participant_survey_answers_collapsed_prep.csv")

# %%
participant_survey_answers_collapsed_prep <- all.data
save(participant_survey_answers_collapsed_prep, file = "participant_survey_answers_collapsed_prep.RData")

# %%
