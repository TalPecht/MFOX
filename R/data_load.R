library(readr)
library(dplyr)
library(stringr)

read_mfox <- function(name) {
  path <- file.path("data", paste0(name, ".csv"))
  if (!file.exists(path)) stop("Missing MFOX table: ", path)
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

load_mfox_data <- function() {
  x <- list(
    studies = read_mfox("studies"),
    cohorts = read_mfox("cohorts"),
    biospecimens = read_mfox("biospecimens"),
    assays = read_mfox("assays"),
    datasets = read_mfox("datasets"),
    registry_studies = read_mfox("registry_studies"),
    registry_sources = read_mfox("registry_sources"),
    projects = read_mfox("projects"),
    project_studies = read_mfox("project_studies"),
    project_companies = read_mfox("project_companies"),
    people_labs = read_mfox("people_labs"),
    people = read_mfox("people"),
    community_publications = read_mfox("community_publications"),
    companies = read_mfox("companies"),
    authorships = read_mfox("authorships"),
    candidates = read_mfox("candidates"),
    exclusions = read_mfox("exclusions"),
    update_log = read_mfox("update_log"),
    controlled_vocab = read_mfox("controlled_vocab")
  )

  x$evidence <- x$assays |>
    left_join(
      x$studies |> select(study_id, title, year, status, journal, first_author,
                          country, population_category, condition, study_design,
                          n_total, longitudinal, source_url, curation_status),
      by = "study_id"
    ) |>
    left_join(
      x$cohorts |> select(cohort_id, cohort_name, n_participants, menstrual_day,
                           collection_device, collection_setting, time_to_processing, clinical_context,
                           preservative, fresh_frozen, paired_blood, paired_endometrium),
      by = "cohort_id"
    )

  x
}
