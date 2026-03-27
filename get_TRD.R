# %%
mdd_first_record <- mdd_data %>% 
select(PatientDurableKey, MDD_Index)

# %%
antidepressant_record <- antidepressant_table  %>%
filter(PatientDurableKey %in% mdd_first_record$PatientDurableKey) %>%
left_join(mdd_first_record, by = join_by(PatientDurableKey == PatientDurableKey)) %>% 
filter(MedicationStartDate >= MDD_Index) # Filter to drugs that are prescribed after first MDD diagnosis

# %%
# "\n\nThere are " %>% cat
# antidepressant_record$PatientDurableKey %>% 
# unique %>% 
# length %>%
# comma %>%
# cat
# " participants in the antidepressant drug dataset." %>% cat
# "\nThe dataset has " %>% cat
# antidepressant_record %>% nrow %>% comma %>% cat
# " antidepressant drug records." %>% cat

# %%
# create a list of all drugs that each participant has taken
antidepressant_record_uq <- antidepressant_record %>% 
dplyr::select(PatientDurableKey, SimpleGenericName) %>% 
unique %>%
arrange(PatientDurableKey)

# %%
filename <- "OutputData/antidepressant_consecutive_instance.csv"
overwrite <- TRUE
if(!file.exists(filename) | overwrite){
    df <- antidepressant_record

    # Calculate intervals and instances
    df <- df %>%
      arrange(PatientDurableKey, SimpleGenericName, MedicationStartDate) %>%
      group_by(PatientDurableKey, SimpleGenericName) %>%
      mutate(
        interval = as.numeric(difftime(MedicationStartDate, lag(MedicationStartDate), units = "days")),
        interval = ifelse(row_number() == 1, 1, interval),
        consecutive_instance = interval > 180,
        consecutive_instance = ifelse(row_number() == 1, TRUE, consecutive_instance),
        consecutive_instance = cumsum(consecutive_instance)
      )

    # Calculate first and last records
    consecutive_instance_tab <- df %>%
      group_by(PatientDurableKey, SimpleGenericName, consecutive_instance) %>%
      summarise(
        first_record = first(MedicationStartDate),
        last_record = last(MedicationStartDate),
        last_record_days_supply = last(DaysSupply),
        drug_class = first(Category_Level_4),
        .groups = 'drop'
      ) %>%
      dplyr::select(-consecutive_instance) %>%
      arrange(PatientDurableKey, first_record, SimpleGenericName) %>%
      mutate(last_record = replace(last_record, last_record == first_record, as.Date(NA))) %>%
      as.data.frame 
      consecutive_instance_tab %>% write.csv(filename, row.names = FALSE)
}else{
    consecutive_instance_tab <- read.csv(filename)
}

# %%
overwrite <- TRUE
filename <- "OutputData/antidepressant_consecutive_period.csv"
if(!file.exists(filename) | overwrite){
    assign_period <- function(this_data){
        for(i in 1:nrow(this_data)){

            this_period_time_from_start <- ifelse(i == 1, 
                                                  0, 
                                                  as.integer(difftime(as_date(this_data$first_record[i]), 
                                                                      as_date(this_data$period_start[i-1]), 
                                                                      units = "days"))
                                                 )

            new_period <- ifelse(i == 1, 
                   TRUE, 
                   (this_period_time_from_start > 365 | (this_data$SimpleGenericName[i] == this_data$SimpleGenericName[i-1]))
                                 )
                                 
            this_data$period[i] = ifelse(i == 1, 
                                         1, 
                                         ifelse(new_period,
                                                this_data$period[i-1] + 1,
                                                this_data$period[i-1]
                                               )
                                         )

            this_data$period_start[i] <- ifelse(new_period, 
                                                this_data$first_record[i], 
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
    split(consecutive_instance_tab$PatientDurableKey)

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
consecutive_period_tab_summ <- consecutive_period_tab %>% group_by(PatientDurableKey, period) %>% 
summarise(
    ndrug = (SimpleGenericName %>% unique %>% length),
    drugs = (SimpleGenericName %>% unique %>% paste(collapse = ", ")),
    period_start = min(first_record), 
    period_end = max(first_record))

# %%
write.csv(consecutive_period_tab_summ, "OutputData/antidepressant_consecutive_period_tab_summ.csv")

# %%
consecutive_period_maxDrugs <- consecutive_period_tab_summ %>% group_by(PatientDurableKey) %>% 
        filter(ndrug == max(ndrug)) %>% filter(period == max(period))
consecutive_period_maxDrugs$has_TRD <- consecutive_period_maxDrugs$ndrug > 2
write.csv(consecutive_period_maxDrugs, "OutputData/antidepressant_consecutive_period_maxDrugs.csv")

# %%
TRD_list <- consecutive_period_maxDrugs %>% filter(has_TRD) %>% dplyr::select(PatientDurableKey)
write.csv(data.frame(PatientDurableKey = unique(TRD_list$PatientDurableKey)), "OutputData/IDs-TRD.csv", row.names = F)
