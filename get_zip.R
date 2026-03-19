# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup(packages_to_install = c("bigrquery", "questionr", "tidyverse", "reshape2","data.table",
                                      "scales", "tableone", "tidyr", "dplyr", "stringr", "RCurl", "RJSONIO",
                                      "lubridate", "svMisc", "IRdisplay", "plotly", "RColorBrewer",
                                      "grDevices"))
options(repr.plot.width=15, repr.plot.height=15)

# %%
my_bucket <- Sys.getenv("WORKSPACE_BUCKET")
my_bucket

# %%
#gs_delete("zip_table.csv", my_bucket)
#delete_if_exists_local("zip_table.csv")

# %%
obs_path <- "obs_table.csv"
obs_sql <- paste("
    SELECT
        observation.person_id,
        observation.observation_datetime,
        observation.value_as_string AS zip_obs,

    FROM
        `observation` AS observation     
            WHERE observation_source_concept_id = 1585250 
            AND observation.value_as_string NOT LIKE 'Res%'
        
", sep="")
download_nonexist_data(obs_path, my_bucket, obs_sql, "obs")
obs_table <- read.csv("obs_table.csv")

# %%
obs_table$zip_obs %>% table

# %%
# The automatically generated SQL code from the Data Browser doesn't quite work.
# I have added that it selects more columns, like the zip code and neighborhood properties
# Also, by doing a right join, all participants will be in the final dataset.

zip_path <- "zip_table.csv"
zip_sql <- paste("
    SELECT
        observation.person_id,
        observation.observation_datetime,
        observation.value_as_string AS zip_obs,
        zip_code.zip3,
        zip_code.fraction_assisted_income,
        zip_code.fraction_high_school_edu,
        zip_code.median_income,
        zip_code.fraction_no_health_ins,
        zip_code.fraction_poverty,
        zip_code.fraction_vacant_housing,
        zip_code.deprivation_index,
        zip_code.acs
    FROM
        `zip3_ses_map` AS zip_code
    RIGHT JOIN
        `observation` AS observation     
        ON CAST(SUBSTR(observation.value_as_string,
                            0,
                            STRPOS(observation.value_as_string,
                                    '*') - 1) 
                    AS INT64
                    ) = zip_code.zip3 
            WHERE observation_source_concept_id = 1585250 
            AND observation.value_as_string NOT LIKE 'Res%'
        
", sep="")

download_nonexist_data(zip_path, my_bucket, zip_sql, "zip")

# %%
zip_table <- read.csv("zip_table.csv")
zip_data <- zip_table

# %%
colnames(zip_data)

# %%
zip_data$zip_obs %>% table

# %%
zip_data$zip_obs_num <- zip_data$zip_obs %>% str_sub(1,3) %>% as.numeric

# %%
explore_data(zip_data)

# %%
# There are two participants zip codes that are not in the zip dataset for some reason. These zips are in DC and Guam. This is why I am using zip_obs_num instead.
zip_data[is.na(zip_data$zip3),] %>% arrange(zip_obs)

# %%
state2zip <- read.csv("state2zip3.csv")
state2zip %>% head

# %%
zip_data_merge <- merge(zip_data, state2zip, by.x = "zip_obs_num", by.y = "zip_3", all.x = T, all.y = F)

# %%
zip_data_merge %>% colnames %>% paste(collapse = "','") %>% cat

# %%
zip_data_merge <- zip_data_merge[,c('person_id',
                      'observation_datetime',
                      'zip_obs',
                      'zip_obs_num',
                      'State',
                      'Abbr',
                      'FIPS.State.Numeric.Code',
                      'Region',
                      'Region.Code',
                      'Division',
                      'Division.Code')]
colnames(zip_data_merge) <- c('person_id',
                      'observation_datetime',
                      'zip_obs',
                      'zip_obs_num',
                      'state',
                      'state_abbr',
                      'FIPS_state_numeric_code',
                      'region',
                      'region_code',
                      'division',
                      'division_code')
zip_data_merge

# %%
zip_data_merge$division %>% table

# %%
# All zip codes were successfully translated to region!
zip_data_merge$division %>% is.na %>% sum

# %%
zip_data_merge %>% arrange(zip_obs_num) %>% filter(is.na(division)) %>% summarise(unique(zip_obs_num)) %>% unlist %>% as.numeric

# %%
zip_data_merge$person_id %>% unique %>% length

# %%
zip_data_merge %>% head

# %%
zip_data_merge_fmt <- zip_data_merge %>%
mutate(across(c(person_id), ~as.character(.))) %>%
mutate(across(c(observation_datetime), ~as.Date(.))) %>%
mutate(across(c(zip_obs,zip_obs_num,state,state_abbr,FIPS_state_numeric_code,region,region_code,division,division_code), ~as.factor(.)))

# %%
zip_data_merge_fmt %>% head

# %%
save(zip_data_merge_fmt, file = "participant_zip_state_region.RData")

# %%
# The automatically generated SQL code from the Data Browser doesn't quite work.
# I have added that it selects more columns, like the zip code and neighborhood properties
# Also, by doing a right join, all participants will be in the final dataset.
zip_path <- "zip_table_full.csv"
zip_sql <- paste("
    SELECT *
    FROM
        `zip3_ses_map`
        
", sep="")

download_nonexist_data(zip_path, my_bucket, zip_sql, "zip")

# %%
zip_full_table <- read.csv("zip_table_full.csv")
zip_full_data <- zip_full_table

# %%
colnames(zip_full_data)

# %%
explore_data(zip_full_data)

# %%
person_ext_path <- "person_ext_table.csv"
person_ext_sql <- paste("
    SELECT *   
    FROM person_ext
        
", sep="")
download_nonexist_data(person_ext_path, my_bucket, person_ext_sql, "person_ext")
person_ext_table <- read.csv("person_ext_table.csv")

# %%
person_ext_table$state_of_residence_source_value %>% length

# %%
person_ext_table$state_of_residence_source_value %>% table

# %%
