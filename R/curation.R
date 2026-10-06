library(readr)
library(dplyr)
library(stringr)

mfox_next_id <- function(values, prefix, width = 3) {
  values <- as.character(values)
  nums <- suppressWarnings(as.integer(str_extract(values, "[0-9]+$")))
  nums <- nums[!is.na(nums)]
  n <- if (length(nums)) max(nums) + 1L else 1L
  paste0(prefix, str_pad(n, width = width, pad = "0"))
}

mfox_backup_data <- function() {
  dir.create("data/backups", showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
  invisible(file.copy(files, file.path("data/backups", paste0(stamp, "_", basename(files))), overwrite = FALSE))
}

mfox_write_table <- function(x, name) {
  readr::write_csv(x, file.path("data", paste0(name, ".csv")), na = "")
}

mfox_set_candidate_status <- function(candidate_id, status, reviewer = "", reason = "") {
  x <- read_mfox("candidates")
  i <- match(candidate_id, x$candidate_id)
  if (is.na(i)) stop("Candidate not found: ", candidate_id)
  x$triage_status[i] <- status
  x$reviewer[i] <- reviewer
  x$decision_reason[i] <- reason
  mfox_write_table(x, "candidates")
}

mfox_exclude_candidate <- function(candidate_id, reviewer, reason) {
  x <- load_mfox_data()
  cand <- x$candidates |> filter(candidate_id == !!candidate_id) |> slice(1)
  if (!nrow(cand)) stop("Candidate not found")
  mfox_backup_data()
  exid <- mfox_next_id(x$exclusions$exclusion_id, "EX")
  doi_or_url <- ifelse(!is.na(cand$url) && cand$url != "", cand$url, cand$doi_or_accession)
  row <- tibble(
    exclusion_id = exid,
    title = cand$title,
    doi_or_url = doi_or_url,
    reason = reason,
    date_reviewed = as.character(Sys.Date()),
    reviewer = reviewer
  )
  mfox_write_table(bind_rows(x$exclusions, row), "exclusions")
  mfox_set_candidate_status(candidate_id, "EXCLUDED", reviewer, reason)
  invisible(exid)
}

mfox_defer_candidate <- function(candidate_id, reviewer, reason) {
  mfox_backup_data()
  mfox_set_candidate_status(candidate_id, "DEFERRED", reviewer, reason)
}

mfox_promote_candidate <- function(candidate_id, fields) {
  x <- load_mfox_data()
  cand <- x$candidates |> filter(candidate_id == !!candidate_id) |> slice(1)
  if (!nrow(cand)) stop("Candidate not found")
  mfox_backup_data()

  sid <- mfox_next_id(x$studies$study_id, "S")
  cid <- mfox_next_id(x$cohorts$cohort_id, "C")
  aid <- mfox_next_id(x$assays$assay_id, "A")
  bid <- mfox_next_id(x$biospecimens$biospecimen_id, "B")

  blank_to_na <- function(z) if (is.null(z) || length(z) == 0 || is.na(z) || trimws(as.character(z)) == "") NA_character_ else as.character(z)
  num_or_na <- function(z) suppressWarnings(as.numeric(blank_to_na(z)))

  study_row <- tibble(
    study_id=sid, title=fields$title, year=num_or_na(fields$year), status="Published",
    doi=blank_to_na(fields$doi), pmid=blank_to_na(fields$pmid), journal=blank_to_na(fields$journal),
    first_author=blank_to_na(fields$first_author), country=blank_to_na(fields$country),
    population_category=blank_to_na(fields$population_category), condition=blank_to_na(fields$condition),
    study_design=blank_to_na(fields$study_design), n_total=num_or_na(fields$n_total), n_mf=num_or_na(fields$n_mf),
    n_controls=num_or_na(fields$n_controls), longitudinal=fields$longitudinal,
    primary_aim=blank_to_na(fields$primary_aim), source_url=blank_to_na(fields$source_url),
    curation_status="Curated", date_verified=as.character(Sys.Date()), notes=blank_to_na(fields$notes)
  )

  cohort_row <- tibble(
    cohort_id=cid, study_id=sid, cohort_name=blank_to_na(fields$cohort_name), condition=blank_to_na(fields$condition),
    n_participants=num_or_na(fields$n_total), age_summary=blank_to_na(fields$age_summary), contraception=blank_to_na(fields$contraception),
    menstrual_day=blank_to_na(fields$menstrual_day), collection_device=blank_to_na(fields$collection_device),
    collection_setting=blank_to_na(fields$collection_setting), time_to_processing=blank_to_na(fields$time_to_processing),
    preservative=blank_to_na(fields$preservative), fresh_frozen=blank_to_na(fields$fresh_frozen),
    paired_blood=blank_to_na(fields$paired_blood), paired_endometrium=blank_to_na(fields$paired_endometrium), notes=NA_character_
  )

  assay_row <- tibble(
    assay_id=aid, study_id=sid, cohort_id=cid, sample_fraction=blank_to_na(fields$sample_fraction),
    omics_modality=fields$omics_modality, assay_type=blank_to_na(fields$assay_type), platform=blank_to_na(fields$platform),
    features=blank_to_na(fields$features), granulocytes_assessed=blank_to_na(fields$granulocytes_assessed),
    neutrophils_resolved=blank_to_na(fields$neutrophils_resolved), primary_comparison=blank_to_na(fields$primary_comparison),
    notes=NA_character_, biospecimen_class=fields$biospecimen_class, derivation_class=fields$derivation_class,
    ex_vivo_manipulation=blank_to_na(fields$ex_vivo_manipulation), culture_status=blank_to_na(fields$culture_status),
    passage_number=blank_to_na(fields$passage_number), direct_mf_scope=fields$direct_mf_scope
  )

  bio_row <- tibble(
    biospecimen_id=bid, study_id=sid, cohort_id=cid, assay_id=aid, sample_fraction=blank_to_na(fields$sample_fraction),
    biospecimen_class=fields$biospecimen_class, derivation_class=fields$derivation_class,
    ex_vivo_manipulation=blank_to_na(fields$ex_vivo_manipulation), culture_status=blank_to_na(fields$culture_status),
    passage_number=blank_to_na(fields$passage_number), direct_mf_scope=fields$direct_mf_scope, notes=NA_character_
  )

  mfox_write_table(bind_rows(x$studies, study_row), "studies")
  mfox_write_table(bind_rows(x$cohorts, cohort_row), "cohorts")
  mfox_write_table(bind_rows(x$assays, assay_row), "assays")
  mfox_write_table(bind_rows(x$biospecimens, bio_row), "biospecimens")

  if (!is.null(fields$accession) && !is.na(fields$accession) && trimws(fields$accession) != "") {
    did <- mfox_next_id(x$datasets$dataset_id, "D")
    dataset_row <- tibble(
      dataset_id=did, study_id=sid, assay_id=aid, repository=blank_to_na(fields$repository), accession=fields$accession,
      data_type=blank_to_na(fields$data_type), raw_available=blank_to_na(fields$raw_available),
      processed_available=blank_to_na(fields$processed_available), metadata_available=blank_to_na(fields$metadata_available),
      code_available=blank_to_na(fields$code_available), dataset_url=blank_to_na(fields$dataset_url),
      date_checked=as.character(Sys.Date()), notes=NA_character_
    )
    mfox_write_table(bind_rows(x$datasets, dataset_row), "datasets")
  }

  mfox_set_candidate_status(candidate_id, "INCLUDED", fields$reviewer, paste0("Promoted as ", sid))

  check <- load_mfox_data()
  errs <- validate_mfox(check)
  if (length(errs)) stop("Promotion wrote files but validation failed: ", paste(errs, collapse=" | "))
  list(study_id=sid, cohort_id=cid, assay_id=aid, biospecimen_id=bid)
}
