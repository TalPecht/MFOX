source("R/data_load.R")
source("R/validation.R")
x <- load_mfox_data()
errors <- validate_mfox(x)
if (length(errors)) {
  cat("MFOX validation FAILED\n")
  cat(paste0(" - ", errors, collapse="\n"), "\n")
  quit(status=1)
}
cat("MFOX validation passed.\n")
cat("Studies:", nrow(x$studies), "\n")
cat("Assays:", nrow(x$assays), "\n")
cat("Datasets:", nrow(x$datasets), "\n")
cat("Candidates:", nrow(x$candidates), "\n")
