# MFOX relational schema

Canonical hierarchy:

`Study -> Cohort -> Biospecimen -> Assay -> Dataset`

## IDs
- `study_id`: stable MFOX study/publication identifier, e.g. `S001`
- `cohort_id`: cohort/group identifier, e.g. `C001`
- `biospecimen_id`: assay-linked provenance identifier, e.g. `B001`
- `assay_id`: molecular assay identifier, e.g. `A001`
- `dataset_id`: repository dataset identifier, e.g. `D001`
- `candidate_id`: unreviewed discovery identifier

IDs should never be recycled after public release.

## Source of truth
The files in `data/*.csv` are canonical. Excel files under `import_archive/` are historical/import snapshots only.

## Promotion rule
Automated discovery may create/update candidate records. Promotion from candidate to curated Study/Cohort/Biospecimen/Assay/Dataset records is a human curation action.
