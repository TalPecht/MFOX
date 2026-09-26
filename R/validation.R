validate_mfox <- function(x) {
  errors <- character()
  required <- list(
    studies = c("study_id","title","year","population_category","curation_status"),
    cohorts = c("cohort_id","study_id","condition"),
    assays = c("assay_id","study_id","cohort_id","omics_modality",
               "biospecimen_class","derivation_class","direct_mf_scope"),
    datasets = c("dataset_id","study_id","assay_id","repository","accession"),
    candidates = c("candidate_id","title","triage_status")
  )
  for (nm in names(required)) {
    miss <- setdiff(required[[nm]], names(x[[nm]]))
    if (length(miss)) errors <- c(errors, paste(nm, "missing:", paste(miss, collapse=", ")))
  }
  if (anyDuplicated(x$studies$study_id)) errors <- c(errors, "Duplicate study_id")
  if (anyDuplicated(x$assays$assay_id)) errors <- c(errors, "Duplicate assay_id")
  if (anyDuplicated(x$datasets$dataset_id)) errors <- c(errors, "Duplicate dataset_id")
  orphan_assays <- setdiff(x$assays$study_id, x$studies$study_id)
  if (length(orphan_assays)) errors <- c(errors, paste("Assays reference unknown study IDs:", paste(orphan_assays, collapse=", ")))
  orphan_data <- setdiff(x$datasets$assay_id, x$assays$assay_id)
  if (length(orphan_data)) errors <- c(errors, paste("Datasets reference unknown assay IDs:", paste(orphan_data, collapse=", ")))
  errors
}
