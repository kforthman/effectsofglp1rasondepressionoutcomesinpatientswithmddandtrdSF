# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup(packages_to_install = c("bigrquery", "questionr", "tidyverse", "reshape2","data.table",
                                       "repr","readr","Hmisc","ggrepel","ggsci",
                                       "rms","ggpubr","dplyr", "stringr"))
# Grab the bucket name
my_bucket <- Sys.getenv("WORKSPACE_BUCKET")

# %%
overwrite <- TRUE

# %%
options(repr.plot.width=15, repr.plot.height=15)

# %%
dataset <- Sys.getenv('WORKSPACE_CDR')
dataset

# %%
download_data <- function(query) {
    tb <- bq_project_query(Sys.getenv('GOOGLE_PROJECT'), query)
    bq_table_download(tb)
}

# %%
## From example workspace, modified to get a list of pids with any survey data (instead of a count)
## Count participants with at least one answer to any Survey
# create the SQL query
query <- str_glue("
SELECT 
DISTINCT person_id 
FROM 
`{dataset}.concept` 
JOIN `{dataset}.concept_ancestor` ON (concept_id = ancestor_concept_id)
JOIN `{dataset}.observation` ON (descendant_concept_id = observation_concept_id)
JOIN `{dataset}.observation_ext` using(observation_id)
WHERE 
concept_name IN ('The Basics', 'Family History', 'Lifestyle', 'Personal Medical History', 'Overall Health', 'Healthcare Access & Utilization') 
AND questionnaire_response_id IS NOT NULL
")

# fetch the data and pull the data into a dataframe
survey_pids <- download_data(query)

# %%
"N Rows:\n" %>% cat
survey_pids %>% nrow() %>% cat
"\n\nAll person ids:\n" %>% cat
survey_pids[1:20,1] %>% unlist %>% paste(collapse = ", ") %>% cat
"..." %>% cat
"\n\nN Unique Ids:\n" %>% cat
survey_pids %>% unlist %>% unique %>% length %>% cat

# %%
# Get all participants who consented to EHR data collection
query <- str_glue("
SELECT DISTINCT person_id

               FROM `{dataset}.observation`

               WHERE observation_concept_id = 1586099

               AND value_source_concept_id = 1586100 #ConsentPermission_Yes
")
ehr_consent_pids <- download_data(query)

# %%
## From example workspace, modified to get a list of pids instead of a count
## Count participants with any data in one of the EHR domain tables
# create the SQL query
query <- str_glue("
SELECT
   distinct person_id
FROM `{dataset}.measurement` AS m
LEFT JOIN `{dataset}.measurement_ext` AS mm on m.measurement_id = mm.measurement_id
WHERE LOWER(mm.src_id) LIKE 'ehr site%'

UNION DISTINCT

SELECT
   DISTINCT person_id
FROM `{dataset}.condition_occurrence` AS m
LEFT JOIN `{dataset}.condition_occurrence_ext` AS mm on m.condition_occurrence_id = mm.condition_occurrence_id
WHERE LOWER(mm.src_id) LIKE 'ehr site%'

UNION DISTINCT

SELECT
   DISTINCT person_id
FROM `{dataset}.device_exposure` AS m
LEFT JOIN `{dataset}.device_exposure_ext` AS mm on m.device_exposure_id = mm.device_exposure_id
WHERE LOWER(mm.src_id) LIKE 'ehr site%'

UNION DISTINCT

SELECT
   DISTINCT person_id
FROM `{dataset}.drug_exposure` AS m
LEFT JOIN `{dataset}.drug_exposure_ext` AS mm on m.drug_exposure_id = mm.drug_exposure_id
WHERE LOWER(mm.src_id) LIKE 'ehr site%'

UNION DISTINCT

Select
   distinct person_id
from `{dataset}.procedure_occurrence` as m
left join `{dataset}.procedure_occurrence_ext` as mm on m.procedure_occurrence_id = mm.procedure_occurrence_id
where lower(mm.src_id) like 'ehr site%'

union distinct

Select
   distinct person_id
from `{dataset}.visit_occurrence` as m
left join `{dataset}.visit_occurrence_ext` as mm on m.visit_occurrence_id = mm.visit_occurrence_id
where lower(mm.src_id) like 'ehr site%'



")
# fetch the data and pull the data into a dataframe, or "df"
ehr_pids <- download_data(query)

# %%
(!ehr_pids$person_id %in% ehr_consent_pids$person_id) %>% sum

# %%
"N Rows:\n" %>% cat
ehr_pids %>% nrow() %>% cat
"\n\nAll person ids:\n" %>% cat
ehr_pids[1:20,1] %>% unlist %>% paste(collapse = ", ") %>% cat
"..." %>% cat
"\n\nN Unique Ids:\n" %>% cat
ehr_pids %>% unlist %>% unique %>% length %>% cat
save(ehr_pids, file = "ehr_pids_noObs.rds")

# %%
if(!file.exists("all_record_length_noObs_v2.csv") | overwrite){
all_visit_ranges <- NULL

i <- 1
my_progress <- 0
while (i < nrow(ehr_pids)) {
    
    # progress bar --------------------------------------
    old_progress <- my_progress
        my_progress <- round(i / nrow(ehr_pids) * 100, 0)
        if(old_progress < my_progress){
            flush.console()
            cat(paste0(my_progress, "%\t"))
        }
    # ---------------------------------------------------
    
    # Select groups of 1,000 participants
    j <- min(i + 999, nrow(ehr_pids))

    subject_list <- paste('(',paste(ehr_pids$person_id[i:j], collapse = ','), ')')
    query <- str_glue("    
    SELECT 
        m.person_id, MIN(m.visit) AS first_visit, MAX(m.visit) AS last_visit
    FROM(
            (
            SELECT 
                vo.person_id, 
                vo.visit_start_datetime AS visit 
            FROM `{dataset}.visit_occurrence` vo
            LEFT JOIN `{dataset}.visit_occurrence_ext` voe
                ON vo.visit_occurrence_id = voe.visit_occurrence_id
            WHERE LOWER(voe.src_id) LIKE 'ehr site%'
            )
        UNION ALL
            (
            SELECT 
                m.person_id, 
                m.measurement_datetime AS visit 
            FROM `{dataset}.measurement` m
            LEFT JOIN `{dataset}.measurement_ext` me
                ON m.measurement_id = me.measurement_id
            WHERE LOWER(me.src_id) LIKE 'ehr site%'
            )
        UNION All
            (
            SELECT 
                co.person_id, 
                co.condition_start_datetime AS visit 
            FROM `{dataset}.condition_occurrence` co
            LEFT JOIN `{dataset}.condition_occurrence_ext` coe
                ON co.condition_occurrence_id = coe.condition_occurrence_id
            WHERE LOWER(coe.src_id) LIKE 'ehr site%'
            )
        UNION All
            (
            SELECT 
                de.person_id, 
                de.device_exposure_start_datetime AS visit 
            FROM `{dataset}.device_exposure` de
            LEFT JOIN `{dataset}.device_exposure_ext` dee
                ON de.device_exposure_id = dee.device_exposure_id
            WHERE LOWER(dee.src_id) LIKE 'ehr site%'
            )
        UNION All
            (
            SELECT 
                de.person_id, 
                de.drug_exposure_start_datetime AS visit 
            FROM `{dataset}.drug_exposure` de
            LEFT JOIN `{dataset}.drug_exposure_ext` dee
                ON de.drug_exposure_id = dee.drug_exposure_id
            WHERE LOWER(dee.src_id) LIKE 'ehr site%'
            )
        UNION All
            (
            SELECT 
                po.person_id, 
                po.procedure_datetime AS visit 
            FROM `{dataset}.procedure_occurrence` po
            LEFT JOIN `{dataset}.procedure_occurrence_ext` poe
                ON po.procedure_occurrence_id = poe.procedure_occurrence_id
            WHERE LOWER(poe.src_id) LIKE 'ehr site%'
            )
        ) AS m
    WHERE m.person_id IN {subject_list}
    GROUP BY m.person_id
    ")
    
    visit_data <- download_data(query)


    all_visit_ranges <- rbind(all_visit_ranges, visit_data)
    
    i <- min(i + 1000, nrow(ehr_pids))
}
write.csv(all_visit_ranges, "all_record_length_noObs_v2.csv")
}else{
    all_visit_ranges <- read.csv("all_record_length_noObs_v2.csv")[,-1]
}

# %%
all_visit_ranges[all_visit_ranges$person_id == 2858071,]
#visit_data$last_visit[visit_data$person_id == 2612969] %>% nchar
#nchar(visit_data$first_visit) %>% table

# %%
all_visit_ranges$first_visit <- as.Date(all_visit_ranges$first_visit)
all_visit_ranges$last_visit <- as.Date(all_visit_ranges$last_visit)
all_visit_ranges %>% head(10)

# %%
which(ehr_pids$person_id == 2612969) #224356 #test

# %%
# prep.data
#load("imputed_trd_weight_prior_state_v4_intTerms.rds")
#all_visit_ranges <- all_visit_ranges[all_visit_ranges$person_id %in% prep.data$person_id,]

# %%
#test
all_visit_ranges$person_id %>% unique %>% length
all_visit_ranges$person_id %>% length
unique(all_visit_ranges$person_id[duplicated(all_visit_ranges$person_id)])
all_visit_ranges[duplicated(all_visit_ranges$person_id),]

# %%
ggplot(all_visit_ranges) + 
    geom_histogram(aes(x = first_visit)) + 
    theme_bw()
ggplot(all_visit_ranges) + 
    geom_histogram(aes(x = last_visit)) + 
    theme_bw()

# %%
person_df <- read.csv("participant_survey_answers_collapsed.csv")[,-1]
person_df %>% head(5)

# %%
merged_data <- merge(all_visit_ranges, person_df, by = "person_id", all.x = T, all.y = F)

# %%
merged_data$DOB <- as.Date(merged_data$birth_datetime)

# %%
ggplot(merged_data) + 
    geom_point(aes(x = DOB, y = first_visit), 
               size = 0.5, alpha = 0.5) + 
    theme_bw()

# %%
merged_data$Age2023 <- (interval(merged_data$birth_datetime,as.Date('2023-10-01')) / dyears()) %>% floor
merged_data$YearsCoverage2023 <- (interval(merged_data$first_visit, as.Date('2023-10-01')) / dyears()) %>% floor
merged_data$LifeCoverage_percent <- (merged_data$YearsCoverage2023 / merged_data$Age2023) * 100

# %%
merged_data$LifeCoverage_percent[is.na(merged_data$first_visit)] <- 0

# %%
hist(merged_data$LifeCoverage_percent, 
     main = 'Percent Of Life With Data (Full V7 pop)', xlab = 'Percent')

# %%
hist(merged_data$LifeCoverage_percent[merged_data$first_visit < as.Date('2023-10-01') & (merged_data$Age2023 < 30)], 
     main = 'Percent Of Life With Data, Age < 30 (Pop with EHR data)', xlab = 'Percent', xlim = c(0, 100))

# %%
ggplot(merged_data) + geom_bin2d(aes(x = DOB, y = first_visit), size = 0.5, alpha = 0.5) + theme_bw()

# %%
ggplot(merged_data[merged_data$Age2023 < 30,]) + geom_bin2d(aes(x = DOB, y = first_visit), size = 0.5, alpha = 0.5) + theme_bw()

# %%
merged_data <- merged_data %>% 
mutate(ehr_length = ifelse(is.na(first_visit), 
                           0,
                           ifelse(is.na(last_visit), 
                                  1/365.24219,
                                  ifelse(first_visit == last_visit,
                                        1/365.24219,
                                        difftime(as_date(last_visit), as_date(first_visit), units = "days")/365.24219
                                        )
                                 )
                           )
       )

# %%
merged_data$ehr_length %>% hist(xlab = "years", main = "Histogram | EHR length")

# %%
"mean\n" %>% cat
merged_data$ehr_length %>% mean
"\n---------------------\n" %>% cat
"\nsd\n" %>% cat
merged_data$ehr_length %>% sd
"\n---------------------\n" %>% cat
"\nmedian\n" %>% cat
merged_data$ehr_length %>% median
"\n---------------------\n" %>% cat
"\nrange\n" %>% cat
merged_data$ehr_length %>% range

# %%
merged_data

# %%
ehr_length <- merged_data %>% dplyr::select(person_id, first_visit, last_visit, first_survey_date, DOB, 
                                            Age2023, YearsCoverage2023, LifeCoverage_percent, ehr_length) %>%
                mutate(person_id = as.character(person_id))

# %%
ehr_length %>% head

# %%
ehr_length$ehr_length %>% is.na %>% sum
ehr_length$ehr_length %>% min
ehr_length$ehr_length %>% max

# %%
save(ehr_length, file = "ehr_length_noObs_v2.rds")

# %%
