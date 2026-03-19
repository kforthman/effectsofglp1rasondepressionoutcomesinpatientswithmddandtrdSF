# %% [markdown]
# Pull the All of Us survey data
#
# Cloud compute profile |
# cpus: 4, RAM: 26
#
# Compute type |
# Standard VM
#
# $0.27/hr

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup(packages_to_install = c("bigrquery", "questionr", "tidyverse", "reshape2","data.table"))

# %%
my_bucket <- Sys.getenv("WORKSPACE_BUCKET")
print(my_bucket)
my_cmd <- paste0("gsutil ls ", my_bucket)
out <- system(my_cmd, intern = T)
cat(paste(out, collapse = "\n"))

# %%
# Location of the datasets
(CURRENT_DATASET <- Sys.getenv('WORKSPACE_CDR'))

# %%
# Function to download data. Argument is a sql query.
download_data <- function(query) {
    tb <- bq_project_query(Sys.getenv('GOOGLE_PROJECT'), query)
    bq_table_download(tb)
}

# %%
# how many modules in survey data and how many questions in each module?
query=str_glue("SELECT survey, COUNT(DISTINCT question_concept_id) count_question
          FROM `{CURRENT_DATASET}.ds_survey` 
          GROUP BY 1
          ORDER BY count_question")
download_data(query)

# %%
## Grab all of the person table
# The person table has one row per participant and has demographic info including:

# gender_source_value
# gender_concept_id
# race_source_value
# race_source_concept_id
# ethnicity_source_value
# ethnicity_source_concept_id
# sex_at_birth_concept_id
# sex_at_birth_source_concept_id
# sex_at_birth_value

query <- str_glue("
SELECT * 
FROM `{CURRENT_DATASET}.person`
")
person_df <- download_data(query)

# %%
dim(person_df)
names(person_df)
head(person_df)

# %%
#Obtain a all Question IDs from a few surveys
sql <- str_glue("SELECT DISTINCT question_concept_id
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('Personal and Family Health History'
                                                )")
#WHERE survey IN ('Overall Health', 'The Basics', 'Lifestyle')")

questions <- download_data(sql)
head(questions)
dim(questions)

# %%
overwrite <- FALSE

for(j in 1:3){
  if(!file.exists(paste0("combined_table_p2_j",j,".csv")) | overwrite){
    cat("file does not exist. creating...")
    flush.console()
    n <- nrow(questions)
    this_first <- TRUE
    if(j==1){this_num_seq <- 1:202
    }else if(j==2){this_num_seq <- 203:404
    }else if(j==3){this_num_seq <- 405:606
    }
    for(i in this_num_seq){#for(i in c(1,49:53)){
      flush.console()
      cat(paste0(round(i / n, 2)*100, "%\t"))
      
      question_concept_id <-  questions[i,1]
      sql <- str_glue("SELECT * FROM `{CURRENT_DATASET}.ds_survey` WHERE question_concept_id = {question_concept_id}")
      this_question_data <- download_data(sql)
      question <- this_question_data$question[1]
      this_question_data[, as.character(question)] <- this_question_data$answer
      this_question_data_tokeep <- this_question_data[, c('person_id', as.character(question), 'survey_datetime')]
      
      this_question_data_tokeep$survey_datetime <- substr(this_question_data_tokeep$survey_datetime, 1, 10)
      
      this_question_data_tokeep <- this_question_data_tokeep[!is.na(this_question_data_tokeep[,as.character(question)]),]
      
      # if participants can mark more than one option
      if(nrow(this_question_data_tokeep) > nrow(unique(this_question_data_tokeep[,c("person_id", "survey_datetime")]))){
        print(i)
        this_question_data_tokeep$is_true <- 1
        this_question_data_tokeep <- pivot_wider(this_question_data_tokeep,
                                                 names_from = as.character(question), 
                                                 values_from = "is_true", 
                                                 values_fill = 0,
                                                 names_glue = paste0(as.character(question), 
                                                                     ": {`", as.character(question), "`}"),
                                                 values_fn = ~ sum(.x, na.rm = T))
        print(colnames(this_question_data_tokeep))
      }
      
      if(this_first){
        combined_table <- this_question_data_tokeep
        questions_text <- as.character(question)
        this_first <- FALSE
        next
      }
      
      combined_table_dt <- combined_table
      this_question_data_tokeep_dt <- this_question_data_tokeep
      
      combined_table <- merge(combined_table_dt, this_question_data_tokeep_dt, all = TRUE, 
                              by = c('person_id', 'survey_datetime'),
                              allow.cartesian=TRUE)
      questions_text <- c(questions_text, as.character(question))
    }
    
    combined_table_dt <- combined_table
    person_df_dt <- person_df
    
    combined_table <- merge(combined_table_dt, person_df_dt, all = TRUE, by = c('person_id'),
                            allow.cartesian=TRUE)
    write.csv(combined_table, paste0("combined_table_p2_j", j,".csv"), row.names = F)
    cat("done.")
  }else{
    cat("file exists. loading...")
    flush.console()
    combined_table <- read.csv(paste0("combined_table_p2_j",j,".csv"))
    cat("done.")
  }
}

combined_table1 <- read.csv(paste0("combined_table_p2_j",1,".csv"))
combined_table2 <- read.csv(paste0("combined_table_p2_j",2,".csv"))
combined_table3 <- read.csv(paste0("combined_table_p2_j",3,".csv"))
combined_table <- merge(combined_table1, combined_table2, all = TRUE, by = c('person_id'),
                            allow.cartesian=TRUE, suffix = c("",".x"))
combined_table <- merge(combined_table,  combined_table3, all = TRUE, by = c('person_id'),
                            allow.cartesian=TRUE, suffix = c("",".x"))

# %%
combined_table <- combined_table %>% dplyr::select(!ends_with(".x"))

# %%
if(!file.exists("this_mapping_start.csv")){
#For loop over each relevant Question ID to get all responses for that particular question
sql <- str_glue("SELECT DISTINCT question_concept_id,question
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('Personal and Family Health History', 
                                                'Social Determinants of Health')")
this_mapping_qid <- download_data(sql) %>% mutate(name = make.names(question))

    
this_name <- this_mapping_qid$name[1]
    this_a <- unique(combined_table[,this_name])
    this_rows <- cbind(rep(this_name, length(this_a)), this_a)
    
    
for(i in 1:nrow(this_mapping_qid)){
    if(i == 1){this_mapping_start <- this_rows; next}
    
    this_name <- this_mapping_qid$name[i]
    if(!this_name %in% colnames(combined_table)){next}
    this_a <- unique(combined_table[,this_name])
    this_rows <- cbind(rep(this_name, length(this_a)), this_a)
    
    
    
    this_mapping_start <- rbind(this_mapping_start, this_rows)
}

this_mapping_start <- cbind(
    this_mapping_qid$question_concept_id[match(this_mapping_start[,1], this_mapping_qid$name)],
    this_mapping_start)

colnames(this_mapping_start) <- c("question_id","sourcevar","answer")

write.csv(this_mapping_start, "this_mapping_start.csv", row.names = F)
}else{
    this_mapping_start <- read.csv("this_mapping_start.csv")
}

# %%
var_mapping1 <- read.csv("this_mapping_manualEdit_p2.csv")
var_mapping2 <- read.csv("this_mapping_manualEdit_p1_2.csv")
var_mapping <- rbind(var_mapping1,var_mapping2)
head(var_mapping)

# %%
combined_table %>% head

# %%
#create numeric versions of all variables found in variable_mapping.csv
combined_table_num <- combined_table
colnames(combined_table_num) <- make.names(colnames(combined_table_num))

for (i in 1:nrow(var_mapping)){
    sourcevar <- make.names(var_mapping$sourcevar[i])
    if(!sourcevar %in% colnames(combined_table_num)){
        cat(paste0(sourcevar, " is not a colname in the combined table.\n"))
        next}
    destination_var <- paste0(sourcevar, '_Numeric')
    if (!(destination_var %in% names(combined_table_num))){
        combined_table_num[, destination_var] <- NA
    }
    answer <- var_mapping$answer[i]
    answer_numeric <- var_mapping$answer_numeric[i]
    to_fill <- (combined_table_num[, sourcevar] == answer)
    to_fill[is.na(to_fill)] <- FALSE
    combined_table_num[to_fill, destination_var] <- answer_numeric    
    if(is.na(answer) & !is.na(answer_numeric)){
        combined_table_num <- combined_table_num %>%
          mutate(across(all_of(destination_var), ~replace_na(., answer_numeric)))
    }
}

# %%
combined_table_num$Are.you.currently.prescribed.medications.and.or.receiving.treatment.for.kidney.disease.without.dialysis._Numeric %>% is.na %>% sum

# %%
idx <- grepl('Numeric', names(combined_table_num))
names(combined_table_num)[idx]

# %%
#n <- nrow(combined_table_num)
#data.frame(
#    colname = (combined_table_num %>% colnames),  
#    complete = unname(apply(combined_table_num, 2, function(x){round(sum(!is.na(x))/n,3)}))
#          ) %>% 
#           write.csv("combined_table_colnames.csv")

# %%
# filter to one row per participant id
all.id <- unique(combined_table_num$person_id)
combined_table_num_collapse <- data.frame(person_id = all.id, x = NA)
n <- ncol(combined_table_num)
# This loop goes by column, selecting the most recent not NA log of that variable for each participant
for(i in 3:n){
    flush.console()
    cat(paste0(round(i / n, 2)*100, "%\t"))
    
    test <- data.frame(combined_table_num[,c(1,2,i)])
    colnames(test) <- c("person_id", "date", "var")

    test <- test %>% filter(!is.na(var)) %>% group_by(person_id) %>% filter(date == max(date))
    test <- test[match(all.id,test$person_id),]
    combined_table_num_collapse <- cbind(combined_table_num_collapse, test[,"var"])
}
colnames(combined_table_num_collapse) <- colnames(combined_table_num)

# %%
combined_table_num_collapse %>% head

# %%
combined_table_num %>% head

# %%
combined_table_num$survey_datetime <- as.Date(combined_table_num$survey_datetime, "%Y-%m-%d")
first_survey_date <- combined_table_num %>% 
    group_by(person_id) %>%
    filter(!all(is.na(survey_datetime))) %>% 
    summarize(first_survey_date = min(survey_datetime, na.rm = TRUE))
first_survey_date

# %%
combined_table_num_collapse <- merge(combined_table_num_collapse, first_survey_date, by = 'person_id')

# %%
combined_table_num_collapse$person_id %>% unique %>% length
combined_table_num_collapse %>% nrow
combined_table_num_collapse %>% ncol
combined_table_num %>% ncol

# %%
combined_table_num_collapse %>% head(10)

# %%
write.csv(combined_table_num, "participant_survey_answers_p2.csv")

# %%
write.csv(combined_table_num_collapse, "participant_survey_answers_p2_collapsed.csv")

# %%
#Obtain a all Question IDs from a few surveys
sql <- str_glue("SELECT DISTINCT question_concept_id
                                           FROM `{CURRENT_DATASET}.ds_survey` 
                                            WHERE survey IN ('Personal and Family Health History', 
                                                'Social Determinants of Health')")
#WHERE survey IN ('Overall Health', 'The Basics', 'Lifestyle')")

questions <- download_data(sql)
head(questions)
dim(questions)

# %%
sql <- str_glue("SELECT *
          FROM {CURRENT_DATASET}.ds_survey
          WHERE survey IN ('Personal and Family Health History')
          LIMIT 100"
          )
data_infos <- download_data(sql)
data_infos

# %%
combined_table %>% colnames

# %%
combined_table_num_collapse$Are.you.currently.prescribed.medications.and.or.receiving.treatment.for.kidney.disease.without.dialysis._Numeric %>% is.na %>% sum

# %%
combined_table_num_collapse %>% dim
