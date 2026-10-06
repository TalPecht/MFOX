validate_mfox <- function(x) {
  errors <- character()

  required_tables <- c(
    "studies", "cohorts", "biospecimens", "assays", "datasets", "registry_studies", "registry_sources",
    "projects", "project_studies", "project_companies", "companies",
    "people_labs", "people", "community_publications", "authorships", "candidates", "exclusions", "update_log", "controlled_vocab"
  )
  missing_tables <- setdiff(required_tables, names(x))
  if (length(missing_tables) > 0) {
    return(paste("Missing tables:", paste(missing_tables, collapse = ", ")))
  }

  required_columns <- list(
    studies = c("study_id","title","year","population_category","curation_status"),
    cohorts = c("cohort_id","study_id","condition"),
    biospecimens = c("biospecimen_id","study_id","cohort_id","assay_id",
                     "biospecimen_class","derivation_class","direct_mf_scope"),
    assays = c("assay_id","study_id","cohort_id","omics_modality",
               "biospecimen_class","derivation_class","direct_mf_scope"),
    datasets = c("dataset_id","study_id","assay_id","repository","accession"),
    registry_studies = c("registry_id","registry","title","status","clinical_context","sample_type","source_url"),
    registry_sources = c("source_id","registry","region","source_type","website","search_route","priority"),
    projects = c("project_id","project_name","project_type","lead_organization","status","source_url"),
    project_studies = c("project_study_id","project_id","study_id","relationship"),
    project_companies = c("project_company_id","project_id","company_id","relationship"),
    companies = c("company_id","company","website","sample_type","technology","utilization_domain","material_exploited","maturity_level","evidence_status"),
    people_labs = c("person_id","name","study_id","institution","country"),
    people = c("person_id","name","institution","country"),
    community_publications = c("publication_id","title","year","mf_scope"),
    authorships = c("authorship_id","publication_id","person_id","is_first_author","is_last_author","is_corresponding_author","is_equal_contribution"),
    candidates = c("candidate_id","title","triage_status"),
    exclusions = c("exclusion_id","title","reason"),
    update_log = c("update_id","run_date","source"),
    controlled_vocab = c("field","allowed_value")
  )

  for (nm in names(required_columns)) {
    miss <- setdiff(required_columns[[nm]], names(x[[nm]]))
    if (length(miss) > 0)
      errors <- c(errors, paste0(nm, " missing required columns: ",
                                 paste(miss, collapse = ", ")))
  }
  if (length(errors) > 0) return(unique(errors))

  clean_ids <- function(v) {
    v <- as.character(v)
    v[!is.na(v) & trimws(v) != ""]
  }

  check_primary <- function(tbl, col, label) {
    ids <- as.character(tbl[[col]])
    missing <- is.na(ids) | trimws(ids) == ""
    if (any(missing))
      errors <<- c(errors, paste0(label, " has ", sum(missing),
                                  " missing primary ID(s)"))
    ids <- ids[!missing]
    dup <- unique(ids[duplicated(ids)])
    if (length(dup) > 0)
      errors <<- c(errors, paste0(label, " has duplicate ", col, ": ",
                                  paste(dup, collapse = ", ")))
  }

  check_fk <- function(values, valid, label) {
    orphan <- setdiff(clean_ids(values), valid)
    if (length(orphan) > 0)
      errors <<- c(errors, paste0(label, ": ", paste(orphan, collapse = ", ")))
  }

  check_primary(x$studies, "study_id", "Studies")
  check_primary(x$cohorts, "cohort_id", "Cohorts")
  check_primary(x$biospecimens, "biospecimen_id", "Biospecimens")
  check_primary(x$assays, "assay_id", "Assays")
  check_primary(x$datasets, "dataset_id", "Datasets")
  check_primary(x$registry_studies, "registry_id", "Registered studies")
  check_primary(x$registry_sources, "source_id", "Registry sources")
  check_primary(x$projects, "project_id", "Projects")
  check_primary(x$project_studies, "project_study_id", "Project-study links")
  check_primary(x$project_companies, "project_company_id", "Project-company links")
  check_primary(x$companies, "company_id", "Companies")
  check_primary(x$people, "person_id", "People")
  check_primary(x$community_publications, "publication_id", "Community publications")
  check_primary(x$authorships, "authorship_id", "Authorships")
  check_primary(x$candidates, "candidate_id", "Candidates")

  study_ids <- clean_ids(x$studies$study_id)
  cohort_ids <- clean_ids(x$cohorts$cohort_id)
  assay_ids <- clean_ids(x$assays$assay_id)
  person_ids <- clean_ids(x$people$person_id)
  publication_ids <- clean_ids(x$community_publications$publication_id)
  project_ids <- clean_ids(x$projects$project_id)
  company_ids <- clean_ids(x$companies$company_id)

  check_fk(x$cohorts$study_id, study_ids, "Cohorts reference unknown study IDs")
  check_fk(x$assays$study_id, study_ids, "Assays reference unknown study IDs")
  check_fk(x$assays$cohort_id, cohort_ids, "Assays reference unknown cohort IDs")
  check_fk(x$biospecimens$study_id, study_ids, "Biospecimens reference unknown study IDs")
  check_fk(x$biospecimens$cohort_id, cohort_ids, "Biospecimens reference unknown cohort IDs")
  check_fk(x$biospecimens$assay_id, assay_ids, "Biospecimens reference unknown assay IDs")
  check_fk(x$datasets$study_id, study_ids, "Datasets reference unknown study IDs")
  check_fk(x$datasets$assay_id, assay_ids, "Datasets reference unknown assay IDs")
  if("linked_study_id" %in% names(x$registry_studies)) check_fk(x$registry_studies$linked_study_id, study_ids, "Registered studies reference unknown linked study IDs")
  check_fk(x$project_studies$project_id, project_ids, "Project-study links reference unknown project IDs")
  check_fk(x$project_studies$study_id, study_ids, "Project-study links reference unknown study IDs")
  check_fk(x$project_companies$project_id, project_ids, "Project-company links reference unknown project IDs")
  check_fk(x$project_companies$company_id, company_ids, "Project-company links reference unknown company IDs")
  people_study_ids <- unlist(strsplit(clean_ids(x$people_labs$study_id), ";", fixed = TRUE))
  people_study_ids <- trimws(people_study_ids)
  people_study_ids <- people_study_ids[people_study_ids != ""]
  check_fk(people_study_ids, study_ids, "People/labs reference unknown study IDs")
  check_fk(x$authorships$person_id, person_ids, "Authorships reference unknown person IDs")
  check_fk(x$authorships$publication_id, publication_ids, "Authorships reference unknown publication IDs")

  for (nm in c("assays", "biospecimens")) {
    vals <- unique(clean_ids(x[[nm]]$direct_mf_scope))
    invalid <- setdiff(vals, c("Yes", "No"))
    if (length(invalid) > 0)
      errors <- c(errors, paste0(nm, " has invalid direct_mf_scope value(s): ",
                                  paste(invalid, collapse = ", ")))
  }

  unique(errors)
}
