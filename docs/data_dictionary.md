# MFOX data dictionary

## `studies.csv`
One row per primary study/publication record.

Fields: `study_id`, `title`, `year`, `status`, `doi`, `pmid`, `journal`, `first_author`, `country`, `population_category`, `condition`, `study_design`, `n_total`, `n_mf`, `n_controls`, `longitudinal`, `primary_aim`, `source_url`, `curation_status`, `date_verified`, `notes`

## `cohorts.csv`
One row per participant cohort/group represented in a study.

Fields: `cohort_id`, `study_id`, `cohort_name`, `condition`, `n_participants`, `age_summary`, `contraception`, `menstrual_day`, `collection_device`, `collection_setting`, `time_to_processing`, `preservative`, `fresh_frozen`, `paired_blood`, `paired_endometrium`, `notes`

## `biospecimens.csv`
One row per assay-linked specimen provenance record.

Fields: `biospecimen_id`, `study_id`, `cohort_id`, `assay_id`, `sample_fraction`, `biospecimen_class`, `derivation_class`, `ex_vivo_manipulation`, `culture_status`, `passage_number`, `direct_mf_scope`, `notes`

## `assays.csv`
One row per molecular assay applied to a cohort/specimen.

Fields: `assay_id`, `study_id`, `cohort_id`, `sample_fraction`, `omics_modality`, `assay_type`, `platform`, `features`, `granulocytes_assessed`, `neutrophils_resolved`, `primary_comparison`, `notes`, `biospecimen_class`, `derivation_class`, `ex_vivo_manipulation`, `culture_status`, `passage_number`, `direct_mf_scope`

## `datasets.csv`
One row per indexed public molecular dataset/accession.

Fields: `dataset_id`, `study_id`, `assay_id`, `repository`, `accession`, `data_type`, `raw_available`, `processed_available`, `metadata_available`, `code_available`, `dataset_url`, `date_checked`, `notes`

## `registry_studies.csv`
One row per public study-registry record in which menstrual fluid/blood/effluent is a primary, optional or secondary biospecimen. Registry records are kept separate from publication-derived MFOX studies.

Fields: `registry_id`, `registry`, `title`, `status`, `clinical_context`, `condition`, `study_type`, `phase`, `enrollment`, `sponsor`, `country`, `start_date`, `completion_date`, `menstrual_fluid_role`, `sample_type`, `technology`, `mf_relevance`, `linked_study_id`, `source_url`, `last_verified`, `notes`, `other_registrations`, `other_registry_url`

## `people_labs.csv`
Publication-derived people/lab metadata for navigation.

Fields: `person_id`, `name`, `role`, `study_id`, `institution`, `country`, `expertise_tags`, `orcid`, `public_profile_url`, `notes`

## `candidates.csv`
Unreviewed/new records. Not part of curated landscape.

Fields: `candidate_id`, `detected_date`, `source`, `title`, `year`, `doi_or_accession`, `url`, `matched_terms`, `candidate_type`, `triage_status`, `reviewer`, `decision_reason`

## `exclusions.csv`
Reviewed exclusions, retained to prevent rediscovery.

Fields: `exclusion_id`, `title`, `doi_or_url`, `reason`, `date_reviewed`, `reviewer`

## `update_log.csv`
Log of automated/manual discovery runs.

Fields: `update_id`, `run_date`, `source`, `query_version`, `records_found`, `new_candidates`, `duplicates`, `notes`

## `controlled_vocab.csv`
Allowed harmonized terms and definitions.

Fields: `field`, `allowed_value`, `definition`, `analysis_note`
