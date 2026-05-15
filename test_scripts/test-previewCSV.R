ds <- open_dataset("Z:/Project D91201F/PatientMedications_Indices_clean.csv",
                   format = "csv")

ds |>
  head(100) |>
  collect() |>
  dplyr::glimpse()
