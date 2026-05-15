library(arrow)
library(dplyr)

antidepressant_table <- open_dataset("OutputData/PatientMedications_Antidepressants") |>
  collect()

antipsychotics_table <- open_dataset("OutputData/PatientMedications_Antipsychotics") |>
  collect()
gc()
