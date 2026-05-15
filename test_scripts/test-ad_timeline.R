library(arrow)
library(dplyr)
# 
# ad_drug_record <- open_dataset("OutputData-1pctSubsample/PatientMedications_Antidepressants")|>
#   collect()
# 
# lobstr::obj_size(ad_drug_record)

simp_drug_record <- open_dataset("OutputData-1pctSubsample/PatientMedications_ADandAP")|>
  collect()

simp_drug_record <- simp_drug_record %>%
  arrange(PatientDurableKey, SimpleGenericName, MedicationStartDate)

lobstr::obj_size(simp_drug_record)
