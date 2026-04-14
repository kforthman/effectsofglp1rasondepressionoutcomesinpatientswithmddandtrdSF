# get_Hydrochlorothiazide_Treatment_Timeline(): Builds consecutive treatment
# instance table for hydrochlorothiazide (active comparator).
#
# Arguments:
#   hydrochlorothiazide_table_file — Path to RDS file containing filtered/recoded hydrochlorothiazide medication records
#   instance_filename         — Output path for consecutive instance table (.rds)
#   overwrite                 — If TRUE, recompute and overwrite existing file (default: TRUE)
#
# Saves (written to disk, no return value):
#   {instance_filename} — consecutive_instance_tab

get_Hydrochlorothiazide_Treatment_Timeline <- function(
    hydrochlorothiazide_table_file,
    instance_filename,
    overwrite = TRUE
) {
    load(hydrochlorothiazide_table_file)

    simp_drug_record <- hydrochlorothiazide_table %>%
    arrange(PatientDurableKey, SimpleGenericName, MedicationStartDate)

    if(!file.exists(instance_filename) | overwrite){
        df <- simp_drug_record

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
            last_record_end_datetime = last(MedicationEndDate),
            last_record_refills = last(RefillsWritten),
            last_record_days_supply = last(DaysSupply),
            total_collapsed_records = n(),
            .groups = 'drop'
          ) %>%
          dplyr::select(-consecutive_instance) %>%
          arrange(PatientDurableKey, first_record, SimpleGenericName) %>%
          mutate(last_record = replace(last_record, last_record == first_record, as.Date(NA))) %>%
          mutate(
            effective_end = as.Date(ifelse(
              !is.na(last_record_end_datetime),
              last_record_end_datetime,
              ifelse(!is.na(last_record), last_record, first_record) +
                ifelse(!is.na(last_record_days_supply), last_record_days_supply, 30)
            ))
          ) %>%
          as.data.frame
          save(consecutive_instance_tab, file = instance_filename)
    }else{
        load(instance_filename)
    }
}
