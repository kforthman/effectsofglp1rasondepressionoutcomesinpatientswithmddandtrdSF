# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
notebook_setup()

# %%
drug_class <- read.csv("DrugClasses.csv") %>%
mutate(Type = recode(Type, "Other" = "Other Antidepressants")) %>% 
rename(concept_category_level_5 = "Type")

# %%
load("Data_Prepped/Prepped_Data-All_Participants-Antidepressants-Drug_Exposure-Translate.rds", verbose = TRUE)
ad_drug_record <- data_prep %>% 
left_join(drug_class, by = join_by("concept_name_detail" == "Name")) %>%
relocate(concept_category_level_5, .before = concept_category_level_4)
load("Data_Prepped/Prepped_Data-All_Participants-Atypical_Antipsychotics-Drug_Exposure-Translate.rds", verbose = TRUE)
aa_drug_record <- data_prep %>%
mutate(concept_category_level_5 = "Other Antipsychotics") %>%
relocate(concept_category_level_5, .before = concept_category_level_4)

# %%
simp_drug_record <- rbind(ad_drug_record, aa_drug_record)  %>%
arrange(person_id, concept_name_detail, start_datetime) %>%
mutate(
    quantity = as.numeric(quantity),
    days_supply = as.numeric(days_supply)
)

# %%
filename <- "antidepressant_antipsychotic_consecutive_instance.rds"
overwrite <- TRUE
if(!file.exists(filename) | overwrite){
    df <- simp_drug_record

    # Calculate intervals and instances.
    # effective_end is computed per record first so that interval measures
    # the gap from the previous prescription's end to this one's start,
    # rather than start-to-start. This avoids falsely splitting a consecutive
    # instance when a long prescription is followed immediately by a refill.
    df <- df %>%
      arrange(person_id, concept_name_detail, start_datetime) %>%
      mutate(
        effective_end = as.Date(ifelse(
          !is.na(end_datetime) & end_datetime > start_datetime,
          end_datetime,
          start_datetime + ifelse(!is.na(days_supply), days_supply, 30)
        ))
      ) %>%
      group_by(person_id, concept_name_detail) %>%
      mutate(
        interval = as.numeric(difftime(start_datetime, lag(start_datetime), units = "days")),
        interval = ifelse(row_number() == 1, 1, interval),
        consecutive_instance = interval > 180,
        consecutive_instance = ifelse(row_number() == 1, TRUE, consecutive_instance),
        consecutive_instance = cumsum(consecutive_instance)
      ) %>% 
    relocate(effective_end, .after = "end_datetime")

    # Calculate first and last records
    consecutive_instance_tab <- df %>%
      group_by(person_id, concept_name_detail, consecutive_instance) %>%
      summarise(
        first_record = first(start_datetime),
        last_record = last(start_datetime),
        last_record_end_datetime = last(end_datetime),
        last_record_refills = last(refills),
        last_record_quantity = last(quantity),
        last_record_days_supply = last(days_supply),
        effective_end = last(effective_end),
        total_collapsed_records = n(),
        concept_category_level_3 = first(concept_category_level_3),
        concept_category_level_4 = first(concept_category_level_4),
        concept_category_level_5 = first(concept_category_level_5),
        concept_atc_code = first(concept_atc_code),
        .groups = 'drop'
      ) %>%
      dplyr::select(-consecutive_instance) %>%
      arrange(person_id, first_record, concept_name_detail) %>%
      mutate(last_record = replace(last_record, last_record == first_record, as.Date(NA))) %>%
      as.data.frame
      save(consecutive_instance_tab, file = filename)
}else{
    load(filename)
}

# %%
overwrite <- TRUE
filename <- "antidepressant_antipsychotic_consecutive_period.rds"
if(!file.exists(filename) | overwrite){
    assign_period <- function(this_data){
        n <- nrow(this_data)
        this_data$period                 <- NA_integer_
        this_data$period_start           <- as.Date(NA)
        this_data$period_time_from_start <- NA_real_
        # period_max_end tracks the running maximum effective_end across all drugs in the current
        # period. Using the max (rather than the immediately preceding row) ensures that a drug
        # can join an existing period even when it does not directly overlap the prior drug,
        # as long as some other drug in the period is still active.
        # Example: Drug A (Apr 2020-Dec 2021), Drug B (May 2020-Jun 2020), Drug C (May 2021-Jul 2021)
        # B and C both overlap A, so all three belong to the same period even though B and C
        # do not overlap each other.
        period_max_end <- as.Date(rep(NA, n))

        # --- Period assignment ---
        for(i in 1:n){
            first_i <- as.Date(this_data$first_record[i])
            end_i   <- this_data$effective_end[i]

            new_period <- if(i == 1) TRUE else first_i > period_max_end[i - 1]

            this_data$period[i]                 <- if(i == 1) 1L else if(new_period) this_data$period[i-1] + 1L else this_data$period[i-1]
            this_data$period_start[i]           <- if(new_period) first_i else this_data$period_start[i-1]
            this_data$period_time_from_start[i] <- if(new_period) 0 else as.integer(difftime(first_i, this_data$period_start[i-1], units = "days"))
            period_max_end[i]                   <- if(new_period) end_i else max(period_max_end[i-1], end_i, na.rm = TRUE)
        }

        # --- Treatment type ---
        # addition:    this drug started while at least one other drug was still active (overlapping windows)
        # switch:      no drug was active at start, but one ended within the prior 180 days
        # new_regimen: no drug was active or ended within the prior 180 days
        this_data$treatment_type                     <- NA_character_
        this_data$ref_drug_name                      <- NA_character_
        this_data$ref_effective_end                  <- as.Date(NA)
        this_data$different_chemical_subgroup        <- NA   # concept_category_level_5 differs from ref
        this_data$different_pharmacological_subgroup <- NA   # concept_category_level_3 differs from ref
        this_data$change_index                       <- NA_integer_

        all_first  <- as.Date(this_data$first_record)
        all_end    <- this_data$effective_end
        all_level3 <- this_data$concept_category_level_3
        all_level5 <- this_data$concept_category_level_5
        all_names  <- this_data$concept_name_detail

        ref_i_vec <- rep(NA_integer_, n)

        for(i in 1:n){
            this_first <- all_first[i]
            other_idx  <- setdiff(seq_len(n), i)

            if(length(other_idx) == 0){
                this_data$treatment_type[i] <- "new_regimen"
                next
            }

            o_first <- all_first[other_idx]
            o_end   <- all_end[other_idx]

            # Drugs that were already active when this drug started.
            # Strict < excludes same-day starts so they don't all become "addition".
            active_mask <- o_first < this_first & o_end >= this_first

            # Among drugs starting on the same day, treat those with a lower row
            # index (already processed) as an arbitrary prior reference, so exactly
            # one same-day drug receives new_regimen/switch and the rest get addition.
            same_day_earlier <- o_first == this_first & other_idx < i

            if(any(active_mask) | any(same_day_earlier)){
                this_data$treatment_type[i] <- "addition"
                # Reference: most recently started drug among active + same-day-earlier
                combined_mask <- active_mask | same_day_earlier
                ref_i_vec[i]  <- other_idx[combined_mask][which.max(o_first[combined_mask])]
            } else {
                # Drugs whose effective window ended within 180 days before this drug started
                recent_mask <- o_end >= (this_first - 180) & o_end < this_first
                if(any(recent_mask)){
                    this_data$treatment_type[i] <- "switch"
                    # Reference: most recently ended drug
                    ref_i_vec[i] <- other_idx[recent_mask][which.max(o_end[recent_mask])]
                } else {
                    this_data$treatment_type[i] <- "new_regimen"
                }
            }

            if(!is.na(ref_i_vec[i])){
                ref_i <- ref_i_vec[i]
                this_data$ref_drug_name[i]                       <- all_names[ref_i]
                this_data$ref_effective_end[i]                   <- all_end[ref_i]
                this_data$different_chemical_subgroup[i]         <- all_level5[i] != all_level5[ref_i]
                this_data$different_pharmacological_subgroup[i]  <- all_level3[i] != all_level3[ref_i]
            }
        }

        # --- Change index ---
        # Counts cumulative additions and switches within each regimen; resets on new_regimen.
        # new_regimen rows receive NA.
        idx_counter <- 0L
        for(i in 1:n){
            if(this_data$treatment_type[i] == "new_regimen"){
                idx_counter <- 0L
            } else {
                idx_counter <- idx_counter + 1L
                this_data$change_index[i] <- idx_counter
            }
        }

        return(this_data)
    }

    consecutive_period_tab_start <- consecutive_instance_tab %>%
    mutate(
        period = NA,
        period_start = NA,
        period_time_from_start = NA
    ) %>%
    split(consecutive_instance_tab$person_id)

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

    # --- Add removal rows ---
    # A removal occurs at the effective_end of a drug instance when that ending
    # is NOT the source of a subsequent switch. We use the ref_effective_end
    # column (added above) to match switch rows back to the exact drug instance
    # they switched from, avoiding false matches when the same drug appears
    # multiple times within the 180-day look-back window.
    switch_sources <- consecutive_period_tab %>%
        filter(treatment_type == "switch") %>%
        transmute(
            person_id,
            switched_from_drug = ref_drug_name,
            switched_from_end  = as.Date(ref_effective_end)
        ) %>%
        distinct()

    removal_rows <- consecutive_period_tab %>%
        left_join(
            switch_sources %>% mutate(is_switched_from = TRUE),
            by = c("person_id",
                   "concept_name_detail" = "switched_from_drug",
                   "effective_end"       = "switched_from_end")
        ) %>%
        filter(!replace_na(is_switched_from, FALSE)) %>%
        transmute(
            person_id,
            concept_name_detail,
            concept_category_level_3,
            concept_category_level_4,
            concept_category_level_5,
            concept_atc_code,
            first_record           = as.Date(effective_end),
            effective_end          = as.Date(NA),
            period,
            period_start,
            period_time_from_start,
            treatment_type         = "removal"
        )

    consecutive_period_tab <- consecutive_period_tab %>%
        bind_rows(removal_rows) %>%
        arrange(person_id, first_record, concept_name_detail)

    save(consecutive_period_tab, file = filename)
}else{
    load(filename)
}

# %%
