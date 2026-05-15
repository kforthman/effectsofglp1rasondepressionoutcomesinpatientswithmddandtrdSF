library(arrow)
library(dplyr)

# # Step 1: Read as all strings, no schema, just null handling
# ds <- open_dataset(
#   "Z:/Project D91201F/PatientMedications_Antidepressants.csv",
#   format = "csv",
#   convert_options = CsvConvertOptions$create(
#     null_values = c("", "NA", "NULL", "null", "*Unspecified"),
#     strings_can_be_null = TRUE
#   )
# )

ds <- open_dataset("OutputData/PatientMedications_Antidepressants")

# Step 2: Peek at what's actually in the problem columns
ds |>
  head(100) |>
  collect() |>
  dplyr::glimpse()

# Step 3: Check for non-integer values in Quantity, weird date formats, etc.
ds |>
  dplyr::count(Category_Level_5_int) |>
  collect() |>
  head(20)

ds |>
  dplyr::count(Category_Level_3_int) |>
  collect() |>
  head(20)

ds |>
  dplyr::count(SimpleGenericName) |>
  collect() |>
  head(20)

ds <- open_dataset("OutputData/PatientMedications_Antipsychotics")

# Step 2: Peek at what's actually in the problem columns
ds |>
  head(100) |>
  collect() |>
  dplyr::glimpse()

# Step 3: Check for non-integer values in Quantity, weird date formats, etc.
ds |>
  dplyr::count(Category_Level_5_int) |>
  collect() |>
  head(20)

ds |>
  dplyr::count(Category_Level_3_int) |>
  collect() |>
  head(20)

ds |>
  dplyr::count(SimpleGenericName) |>
  collect() |>
  head(20)