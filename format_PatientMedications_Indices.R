# This file removes the unnecessary header in the PatientMedications_Indices.csv

input_path  <- "Z:/Project D91201F/PatientMedications_Indices.csv"
output_path <- "Z:/Project D91201F/PatientMedications_Indices_clean.csv"

# Use a temp file so the original isn't corrupted if something goes wrong
temp_path <- tempfile(fileext = ".csv")

con_in  <- file(input_path, "r")
con_out <- file(temp_path, "w")

# Skip the first 11 lines
readLines(con_in, n = 11)

# Stream the rest in chunks
chunk_size <- 100000
repeat {
  lines <- readLines(con_in, n = chunk_size)
  if (length(lines) == 0) break
  writeLines(lines, con_out)
}

close(con_in)
close(con_out)

# Replace the original only after successful write
file.copy(temp_path, output_path, overwrite = TRUE)
file.remove(temp_path)
