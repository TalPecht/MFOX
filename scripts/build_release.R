library(readr)
source("R/data_load.R")
source("R/validation.R")

x <- load_mfox_data()
errors <- validate_mfox(x)
if (length(errors)) stop(paste(errors, collapse="\n"))

dir.create("release", showWarnings=FALSE)
files <- list.files("data", pattern="\\.csv$", full.names=TRUE)
file.copy(files, "release", overwrite=TRUE)
writeLines(c(
  paste0("MFOX release built: ", Sys.Date()),
  paste0("Studies: ", nrow(x$studies)),
  paste0("Assays: ", nrow(x$assays)),
  paste0("Datasets: ", nrow(x$datasets))
), "release/RELEASE_SUMMARY.txt")
cat("Release snapshot created in release/\n")
