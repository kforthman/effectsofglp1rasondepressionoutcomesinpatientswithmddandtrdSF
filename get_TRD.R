# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("data_prep_functions.txt")
notebook_setup()

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Depression-Condition_Occurrence.rds")
mdd_record <- data_prep
mdd_record %>% head

# %%
mdd_first_record <- mdd_record %>% 
group_by(person_id) %>% 
filter(start_datetime == min(start_datetime)) %>% 
dplyr::select(person_id, start_datetime) %>%
rename(first_depression_dx_date = start_datetime) %>%
slice(1) %>%
ungroup()

mdd_first_record %>% head
mdd_first_record %>% nrow

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Antidepressants-Drug_Exposure.rds")
antidepressant_record <- data_prep  %>%
filter(person_id %in% mdd_first_record$person_id) %>%
left_join(mdd_first_record, by = join_by(person_id == person_id)) %>% # Filter to drugs that are prescribed after first MDD diagnosis
filter(start_datetime >= first_depression_dx_date)

antidepressant_record %>%
relocate(c(concept_name_detail, start_datetime, end_datetime, first_depression_dx_date), .after = 2) %>% 
head
antidepressant_record %>% dim

# %%
antidepressant_record %>% head %>% print_all_cols

# %% [markdown]
# # Drug consecutive instance

# %%
"\n\nThere are " %>% cat
antidepressant_record$person_id %>% 
unique %>% 
length %>%
comma %>%
cat
" participants in the antidepressant drug dataset." %>% cat
"\nThe dataset has " %>% cat
antidepressant_record %>% nrow %>% comma %>% cat
" antidepressant drug records." %>% cat

# %%
# create a list of all drugs that each participant has taken
antidepressant_record_uq <- antidepressant_record %>% 
dplyr::select(person_id, concept_name_detail) %>% 
unique %>%
arrange(person_id)

antidepressant_record_uq %>% head

# %%
filename <- "antidepressant_consecutive_instance.csv"
overwrite <- TRUE
if(!file.exists(filename) | overwrite){
    df <- antidepressant_record

    # Calculate intervals and instances
    df <- df %>%
      arrange(person_id, concept_name_detail, start_datetime) %>%
      group_by(person_id, concept_name_detail) %>%
      mutate(
        interval = as.numeric(difftime(start_datetime, lag(start_datetime), units = "days")),
        interval = ifelse(row_number() == 1, 1, interval),
        consecutive_instance = interval > 180,
        consecutive_instance = ifelse(row_number() == 1, TRUE, consecutive_instance),
        consecutive_instance = cumsum(consecutive_instance)
      )

    # Calculate first and last records
    consecutive_instance_tab <- df %>%
      group_by(person_id, concept_name_detail, consecutive_instance) %>%
      summarise(
        first.record = first(start_datetime),
        last.record = last(start_datetime),
        last.record.days_supply = last(days_supply),
        drug_class = first(concept_category_level_4),
        .groups = 'drop'
      ) %>%
      dplyr::select(-consecutive_instance) %>%
      arrange(person_id, first.record, concept_name_detail) %>%
      mutate(last.record = replace(last.record, last.record == first.record, as.Date(NA))) %>%
      rename(person.id = person_id) %>%
      as.data.frame 
      consecutive_instance_tab %>% write.csv(filename, row.names = FALSE)
}else{
    consecutive_instance_tab <- read.csv(filename)
}

# %%
consecutive_instance_tab %>% head

# %%
overwrite <- TRUE
my_progress <- 0
filename <- "antidepressant_consecutive_period.csv"
if(!file.exists(filename) | overwrite){
    assign_period <- function(this_data){
        for(i in 1:nrow(this_data)){

            this_period_time_from_start <- ifelse(i == 1, 
                                                  0, 
                                                  as.integer(difftime(as_date(this_data$first.record[i]), 
                                                                      as_date(this_data$period_start[i-1]), 
                                                                      units = "days"))
                                                 )

            new_period <- ifelse(i == 1, 
                   TRUE, 
                   (this_period_time_from_start > 365 | (this_data$concept_name_detail[i] == this_data$concept_name_detail[i-1]))
                                 )
                                 
            this_data$period[i] = ifelse(i == 1, 
                                         1, 
                                         ifelse(new_period,
                                                this_data$period[i-1] + 1,
                                                this_data$period[i-1]
                                               )
                                         )

            this_data$period_start[i] <- ifelse(new_period, 
                                                this_data$first.record[i], 
                                                this_data$period_start[i-1])

            this_data$period_time_from_start[i] <- ifelse(new_period, 
                                                  0, 
                                                  this_period_time_from_start)                       
            }

        return(this_data)
    }

    consecutive_period_tab_start <- consecutive_instance_tab %>%
    mutate(
        period = NA,
        period_start = NA,
        period_time_from_start = NA
    ) %>%
    split(consecutive_instance_tab$person.id)

    # Register the parallel backend
    num_cores <- detectCores()
    cl <- makeCluster(num_cores/2)
    registerDoParallel(cl)

    # Use foreach to apply your function in parallel
    consecutive_period_tab_list <- foreach(x=consecutive_period_tab_start, .packages = c("lubridate","base")) %dopar% {
        assign_period(x)
    }

    # Stop the cluster when you're done
    stopCluster(cl)

    consecutive_period_tab <- bind_rows(consecutive_period_tab_list)
    consecutive_period_tab %>% write.csv(filename, row.names = F)
}else{
    consecutive_period_tab <- read.csv(filename)
}

# %%
consecutive_period_tab %>% head(35)

# %%
consecutive_period_tab$drug_class %>% table
consecutive_period_tab$drug_class %>% is.na %>% sum

# %%
write.csv(consecutive_period_tab, "antidepressant_consecutive_period_tab.csv")

# %%
consecutive_period_tab_summ <- consecutive_period_tab %>% group_by(person.id,period) %>% 
summarise(
    ndrug = (concept_name_detail %>% unique %>% length),
    drugs = (concept_name_detail %>% unique %>% paste(collapse = ", ")),
    period.start = min(first.record), period.end = max(first.record))
consecutive_period_tab_summ %>% head(10)

# %%
write.csv(consecutive_period_tab_summ, "antidepressant_consecutive_period_tab_summ.csv")

# %%
consecutive_period_maxDrugs <- consecutive_period_tab_summ %>% group_by(person.id) %>% 
        filter(ndrug == max(ndrug)) %>% filter(period == max(period))
consecutive_period_maxDrugs$has_TRD <- consecutive_period_maxDrugs$ndrug > 2
write.csv(consecutive_period_maxDrugs, "antidepressant_consecutive_period_maxDrugs.csv")
consecutive_period_maxDrugs

# %%
consecutive_period_maxDrugs$ndrug %>% table

# %%
cat(paste0("N TRD: ",(consecutive_period_maxDrugs$has_TRD) %>% sum, "\n"))
cat(paste0("N Not TRD: ",(!consecutive_period_maxDrugs$has_TRD) %>% sum))

# %%
TRD_list <- consecutive_period_maxDrugs %>% filter(has_TRD) %>% dplyr::select(person.id)
write.table(unique(TRD_list$person.id), "Data_Prepped/IDs-TRD.txt", row.names = F, col.names = F)

# %%
bpd_ids <- read.csv("Data_Prepped/IDs-Bipolar_Disorder-Condition_Occurrence.txt", header = F) %>% rename(person_id = V1)
sch_ids <- read.csv("Data_Prepped/IDs-Schizophrenia-Condition_Occurrence.txt", header = F) %>% rename(person_id = V1)

trd_id <- data.frame(person_id = unique(TRD_list$person.id))

trd_no_bpd_sch_id <- data.frame(person_id = trd_id$person_id[!trd_id$person_id %in% c(bpd_ids$person_id, sch_ids$person_id)])
cat(paste0("N TRD: ",(trd_no_bpd_sch_id) %>% nrow, "\n"))

# %%
v7_ids <- read.csv("IDs-All_IDs_v7.txt", header = F) %>% rename(person_id = V1)

trd_no_bpd_sch_v7_id <- data.frame(person_id = trd_no_bpd_sch_id$person_id[trd_no_bpd_sch_id$person_id %in% v7_ids$person_id])
cat(paste0("N TRD: ",(trd_no_bpd_sch_v7_id) %>% nrow, "\n"))

# %%
