out <- diag_table %>%
  filter(Diagnosis == "MDD") %>%
  rename(MDD_Index_from_FirstDiagnosis = "FirstDiagnosisDate") %>%
  dplyr::select(-Diagnosis) %>%
  full_join(mdd_data %>% dplyr::select(PatientDurableKey, MDD_Index), by = "PatientDurableKey") %>%
  rename(MDD_Index_from_MDDPatientList = "MDD_Index") %>%
  filter(!is.na(MDD_Index_from_FirstDiagnosis) | !is.na(MDD_Index_from_MDDPatientList))

out %>% count(MDD_Index_from_FirstDiagnosis == MDD_Index_from_MDDPatientList)
out %>% count(is.na(MDD_Index_from_FirstDiagnosis))
out %>% count(is.na(MDD_Index_from_MDDPatientList))
