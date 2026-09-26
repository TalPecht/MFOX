# ============================================================
# MFOX — Database validation functions
# File: R/validation.R
# ============================================================
#
# Purpose:
# Validate the internal structure of the MFOX CSV database
# before the Shiny app is run or a database release is made.
#
# The function returns a character vector containing errors.
# If the vector has length 0, validation has passed.
# ============================================================


validate_mfox <- function(x) {

  errors <- character()


  # ----------------------------------------------------------
  # 1. Check that required tables exist
  # ----------------------------------------------------------

  required_tables <- c(
    "studies",
    "cohorts",
    "biospecimens",
    "assays",
    "datasets",
    "people_labs",
    "candidates",
    "exclusions",
    "update_log",
    "controlled_vocab"
  )

  missing_tables <- setdiff(required_tables, names(x))

  if (length(missing_tables) > 0) {

    errors <- c(
      errors,
      paste(
        "Missing required tables:",
        paste(missing_tables, collapse = ", ")
      )
    )
  }


  # ----------------------------------------------------------
  # Stop column-level checks if essential tables are missing
  # ----------------------------------------------------------

  essential_tables <- c(
    "studies",
    "cohorts",
    "assays",
    "datasets",
    "candidates"
  )

  if (any(!essential_tables %in% names(x))) {
    return(errors)
  }


  # ----------------------------------------------------------
  # 2. Check required columns
  # ----------------------------------------------------------

  required_columns <- list(

    studies = c(
      "study_id",
      "title",
      "year",
      "population_category",
      "curation_status"
    ),

    cohorts = c(
      "cohort_id",
      "study_id",
      "condition"
    ),

    assays = c(
      "assay_id",
      "study_id",
      "cohort_id",
      "omics_modality",
      "biospecimen_class",
      "derivation_class",
      "direct_mf_scope"
    ),

    datasets = c(
      "dataset_id",
      "study_id",
      "assay_id",
      "repository",
      "accession"
    ),

    candidates = c(
      "candidate_id",
      "title",
      "triage_status"
    )
  )


  for (table_name in names(required_columns)) {

    missing_columns <- setdiff(
      required_columns[[table_name]],
      names(x[[table_name]])
    )

    if (length(missing_columns) > 0) {

      errors <- c(
        errors,
        paste(
          table_name,
          "missing required columns:",
          paste(missing_columns, collapse = ", ")
        )
      )
    }
  }


  # ----------------------------------------------------------
  # Stop relational checks if required ID columns are missing
  # ----------------------------------------------------------

  id_columns_ok <- all(c("study_id") %in% names(x$studies)) &&
    all(c("cohort_id", "study_id") %in% names(x$cohorts)) &&
    all(c("assay_id", "study_id", "cohort_id") %in% names(x$assays)) &&
    all(c("dataset_id", "study_id", "assay_id") %in% names(x$datasets))

  if (!id_columns_ok) {
    return(errors)
  }


  # ----------------------------------------------------------
  # 3. Check primary IDs for missing values
  # ----------------------------------------------------------

  missing_study_ids <- is.na(x$studies$study_id) |
    trimws(x$studies$study_id) == ""

  if (any(missing_study_ids)) {

    errors <- c(
      errors,
      paste(
        "Studies table contains",
        sum(missing_study_ids),
        "missing study_id value(s)."
      )
    )
  }


  missing_cohort_ids <- is.na(x$cohorts$cohort_id) |
    trimws(x$cohorts$cohort_id) == ""

  if (any(missing_cohort_ids)) {

    errors <- c(
      errors,
      paste(
        "Cohorts table contains",
        sum(missing_cohort_ids),
        "missing cohort_id value(s)."
      )
    )
  }


  missing_assay_ids <- is.na(x$assays$assay_id) |
    trimws(x$assays$assay_id) == ""

  if (any(missing_assay_ids)) {

    errors <- c(
      errors,
      paste(
        "Assays table contains",
        sum(missing_assay_ids),
        "missing assay_id value(s)."
      )
    )
  }


  missing_dataset_ids <- is.na(x$datasets$dataset_id) |
    trimws(x$datasets$dataset_id) == ""

  if (any(missing_dataset_ids)) {

    errors <- c(
      errors,
      paste(
        "Datasets table contains",
        sum(missing_dataset_ids),
        "missing dataset_id value(s)."
      )
    )
  }


  # ----------------------------------------------------------
  # 4. Check for duplicate primary IDs
  # ----------------------------------------------------------

  valid_study_ids <- x$studies$study_id[
    !is.na(x$studies$study_id) &
      trimws(x$studies$study_id) != ""
  ]

  duplicated_study_ids <- unique(
    valid_study_ids[duplicated(valid_study_ids)]
  )

  if (length(duplicated_study_ids) > 0) {

    errors <- c(
      errors,
      paste(
        "Duplicate study_id values:",
        paste(duplicated_study_ids, collapse = ", ")
      )
    )
  }


  valid_cohort_ids <- x$cohorts$cohort_id[
    !is.na(x$cohorts$cohort_id) &
      trimws(x$cohorts$cohort_id) != ""
  ]

  duplicated_cohort_ids <- unique(
    valid_cohort_ids[duplicated(valid_cohort_ids)]
  )

  if (length(duplicated_cohort_ids) > 0) {

    errors <- c(
      errors,
      paste(
        "Duplicate cohort_id values:",
        paste(duplicated_cohort_ids, collapse = ", ")
      )
    )
  }


  valid_assay_ids <- x$assays$assay_id[
    !is.na(x$assays$assay_id) &
      trimws(x$assays$assay_id) != ""
  ]

  duplicated_assay_ids <- unique(
    valid_assay_ids[duplicated(valid_assay_ids)]
  )

  if (length(duplicated_assay_ids) > 0) {

    errors <- c(
      errors,
      paste(
        "Duplicate assay_id values:",
        paste(duplicated_assay_ids, collapse = ", ")
      )
    )
  }


  valid_dataset_ids <- x$datasets$dataset_id[
    !is.na(x$datasets$dataset_id) &
      trimws(x$datasets$dataset_id) != ""
  ]

  duplicated_dataset_ids <- unique(
    valid_dataset_ids[duplicated(valid_dataset_ids)]
  )

  if (length(duplicated_dataset_ids) > 0) {

    errors <- c(
      errors,
      paste(
        "Duplicate dataset_id values:",
        paste(duplicated_dataset_ids, collapse = ", ")
      )
    )
  }


  # ----------------------------------------------------------
  # 5. Check Cohort -> Study relationships
  # ----------------------------------------------------------

  cohort_study_ids <- x$cohorts$study_id[
    !is.na(x$cohorts$study_id) &
      trimws(x$cohorts$study_id) != ""
  ]

  orphan_cohort_studies <- setdiff(
    unique(cohort_study_ids),
    unique(valid_study_ids)
  )

  if (length(orphan_cohort_studies) > 0) {

    errors <- c(
      errors,
      paste(
        "Cohorts reference unknown study IDs:",
        paste(orphan_cohort_studies, collapse = ", ")
      )
    )
  }


  # ----------------------------------------------------------
  # 6. Check Assay -> Study relationships
  # ----------------------------------------------------------

  assay_study_ids <- x$assays$study_id[
    !is.na(x$assays$study_id) &
      trimws(x$assays$study_id) != ""
  ]

  orphan_assay_studies <- setdiff(
    unique(assay_study_ids),
    unique(valid_study_ids)
  )

  if (length(orphan_assay_studies) > 0) {

    errors <- c(
      errors,
      paste(
        "Assays reference unknown study IDs:",
        paste(orphan_assay_studies, collapse = ", ")
      )
    )
  }


  # ----------------------------------------------------------
  # 7. Check Assay -> Cohort relationships
  # ----------------------------------------------------------

  assay_cohort_ids <- x$assays$cohort_id[
    !is.na(x$assays$cohort_id) &
      trimws(x$assays$cohort_id) != ""
  ]

  orphan_assay_cohorts <- setdiff(
    unique(assay_cohort_ids),
    unique(valid_cohort_ids)
  )

  if (length(orphan_assay_cohorts) > 0) {

    errors <- c(
      errors,
      paste(
        "Assays reference unknown cohort IDs:",
        paste(orphan_assay_cohorts, collapse = ", ")
      )
    )
  }


  # ----------------------------------------------------------
  # 8. Check Dataset -> Study relationships
  # ----------------------------------------------------------
  #
  # Blank dataset study IDs are ignored here.
  # If you later decide that every dataset MUST have study_id,
  # this can be changed from a relational check to a required
  # field check.
  # ----------------------------------------------------------

  dataset_study_ids <- x$datasets$study_id[
    !is.na(x$datasets$study_id) &
      trimws(x$datasets$study_id) != ""
  ]

  orphan_dataset_studies <- setdiff(
    unique(dataset_study_ids),
    unique(valid_study_ids)
  )

  if (length(orphan_dataset_studies) > 0) {

    errors <- c(
      errors,
      paste(
        "Datasets reference unknown study IDs:",
        paste(orphan_dataset_studies, collapse = ", ")
      )
    )
  }


  # ----------------------------------------------------------
  # 9. Check Dataset -> Assay relationships
  # ----------------------------------------------------------
  #
  # IMPORTANT:
  # Blank / NA assay IDs are ignored.
  #
  # This fixes the previous problem where NA was incorrectly
  # interpreted as an unknown assay ID.
  # ----------------------------------------------------------

  dataset_assay_ids <- x$datasets$assay_id[
    !is.na(x$datasets$assay_id) &
      trimws(x$datasets$assay_id) != ""
  ]

  orphan_dataset_assays <- setdiff(
    unique(dataset_assay_ids),
    unique(valid_assay_ids)
  )

  if (length(orphan_dataset_assays) > 0) {

    errors <- c(
      errors,
      paste(
        "Datasets reference unknown assay IDs:",
        paste(orphan_dataset_assays, collapse = ", ")
      )
    )
  }


  # ----------------------------------------------------------
  # 10. Check biospecimen relationships, if table is available
  # ----------------------------------------------------------

  if ("biospecimens" %in% names(x)) {

    if ("study_id" %in% names(x$biospecimens)) {

      biospecimen_study_ids <- x$biospecimens$study_id[
        !is.na(x$biospecimens$study_id) &
          trimws(x$biospecimens$study_id) != ""
      ]

      orphan_biospecimen_studies <- setdiff(
        unique(biospecimen_study_ids),
        unique(valid_study_ids)
      )

      if (length(orphan_biospecimen_studies) > 0) {

        errors <- c(
          errors,
          paste(
            "Biospecimens reference unknown study IDs:",
            paste(orphan_biospecimen_studies, collapse = ", ")
          )
        )
      }
    }


    if ("cohort_id" %in% names(x$biospecimens)) {

      biospecimen_cohort_ids <- x$biospecimens$cohort_id[
        !is.na(x$biospecimens$cohort_id) &
          trimws(x$biospecimens$cohort_id) != ""
      ]

      orphan_biospecimen_cohorts <- setdiff(
        unique(biospecimen_cohort_ids),
        unique(valid_cohort_ids)
      )

      if (length(orphan_biospecimen_cohorts) > 0) {

        errors <- c(
          errors,
          paste(
            "Biospecimens reference unknown cohort IDs:",
            paste(orphan_biospecimen_cohorts, collapse = ", ")
          )
        )
      }
    }


    if ("assay_id" %in% names(x$biospecimens)) {

      biospecimen_assay_ids <- x$biospecimens$assay_id[
        !is.na(x$biospecimens$assay_id) &
          trimws(x$biospecimens$assay_id) != ""
      ]

      orphan_biospecimen_assays <- setdiff(
        unique(biospecimen_assay_ids),
        unique(valid_assay_ids)
      )

      if (length(orphan_biospecimen_assays) > 0) {

        errors <- c(
          errors,
          paste(
            "Biospecimens reference unknown assay IDs:",
            paste(orphan_biospecimen_assays, collapse = ", ")
          )
        )
      }
    }
  }


  # ----------------------------------------------------------
  # 11. Validate direct_mf_scope
  # ----------------------------------------------------------

  if ("direct_mf_scope" %in% names(x$assays)) {

    direct_scope_values <- unique(
      x$assays$direct_mf_scope[
        !is.na(x$assays$direct_mf_scope) &
          trimws(x$assays$direct_mf_scope) != ""
      ]
    )

    allowed_direct_scope <- c(
      "Yes",
      "No"
    )

    invalid_direct_scope <- setdiff(
      direct_scope_values,
      allowed_direct_scope
    )

    if (length(invalid_direct_scope) > 0) {

      errors <- c(
        errors,
        paste(
          "Invalid direct_mf_scope values:",
          paste(invalid_direct_scope, collapse = ", "),
          ". Allowed values are Yes or No."
        )
      )
    }
  }


  # ----------------------------------------------------------
  # 12. Validate candidate IDs
  # ----------------------------------------------------------

  if ("candidate_id" %in% names(x$candidates)) {

    candidate_ids <- x$candidates$candidate_id[
      !is.na(x$candidates$candidate_id) &
        trimws(x$candidates$candidate_id) != ""
    ]

    duplicate_candidate_ids <- unique(
      candidate_ids[duplicated(candidate_ids)]
    )

    if (length(duplicate_candidate_ids) > 0) {

      errors <- c(
        errors,
        paste(
          "Duplicate candidate_id values:",
          paste(duplicate_candidate_ids, collapse = ", ")
        )
      )
    }
  }


  # ----------------------------------------------------------
  # 13. Return validation results
  # ----------------------------------------------------------

  return(unique(errors))
}
