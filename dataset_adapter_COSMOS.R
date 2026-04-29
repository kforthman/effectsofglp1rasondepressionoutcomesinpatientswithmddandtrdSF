# dataset_adapter_COSMOS.R
# Dataset-specific settings for the COSMOS Sandbox data source.
# Sourced by prepData.R immediately after loading config.
#
# diag_table arrives in WIDE format (PatientDurableKey + one *_FirstDiagnosis
# column per diagnosis) and is joined directly to mdd_data without pivoting.
#
# treatment_overlap_table does not exist as a separate source; overlap columns
# are already embedded in dte_cohort_data.

diag_table_format           <- "wide"
has_treatment_overlap_table <- FALSE
