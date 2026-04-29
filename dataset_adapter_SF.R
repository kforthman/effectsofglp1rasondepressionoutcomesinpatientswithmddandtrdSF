# dataset_adapter_SF.R
# Dataset-specific settings for the SF (Epic/Caboodle) data source.
# Sourced by prepData.R immediately after loading config.
#
# diag_table arrives in LONG format (PatientDurableKey, Diagnosis,
# FirstDiagnosisDate) and must be pivot_wider'd before joining to mdd_data.
#
# treatment_overlap_table is a separate source table that must be read and
# joined into dte_cohort_data.

diag_table_format           <- "long"
has_treatment_overlap_table <- TRUE
