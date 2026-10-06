source("R/data_load.R")
source("R/validation.R")

cat("\n========================================\n")
cat(" MFOX database validation\n")
cat("========================================\n\n")

x <- load_mfox_data()
errors <- validate_mfox(x)

if (length(errors) > 0) {
  cat("MFOX validation FAILED\n\n")
  for (error in errors) cat(" - ", error, "\n", sep = "")
  cat("\n")
  quit(save = "no", status = 1)
} else {
  cat("MFOX validation PASSED\n\n")
  cat("Database summary:\n")
  cat("  Studies:      ", nrow(x$studies), "\n", sep = "")
  cat("  Cohorts:      ", nrow(x$cohorts), "\n", sep = "")
  cat("  Biospecimens: ", nrow(x$biospecimens), "\n", sep = "")
  cat("  Assays:       ", nrow(x$assays), "\n", sep = "")
  cat("  Datasets:     ", nrow(x$datasets), "\n", sep = "")
  cat("  Registered:   ", nrow(x$registry_studies), "\n", sep = "")
  cat("  Projects:     ", nrow(x$projects), "\n", sep = "")
  cat("  Project links:", nrow(x$project_studies) + nrow(x$project_companies), "\n", sep = "")
  cat("  Candidates:   ", nrow(x$candidates), "\n", sep = "")

# Syntax-check the Shiny application as part of release validation.
parse(file = "app.R")
cat("  app.R syntax:  PASS\n")
  cat("\nNo structural database errors detected.\n\n")
}
