# MFOX relational schema — v1.4.1

MFOX separates the **scientific evidence chain** from the **knowledge/context layer**.

## Scientific evidence chain

`Study -> Cohort -> Biospecimen -> Assay -> Dataset`

A Study is the structured scientific record that anchors cohorts, sample provenance and assays. A publication is represented in `community_publications.csv` and can point to the same `study_id`. Datasets remain external repository records; MFOX stores identifiers, availability metadata and source links rather than hosting omics files.

## Connected knowledge layer

`Project / Program <-> Study` via `project_studies.csv`

`Project / Program <-> Company` via `project_companies.csv`

`Study <-> Publication <-> Authorship <-> Person / Institution` via `community_publications.csv` and `authorships.csv`.

Relationships must be explicit and curator-verifiable. MFOX should leave a relationship blank rather than infer that a paper, dataset, company or lab belongs to a project.

## IDs
- `study_id`: stable MFOX study identifier, e.g. `S001`
- `cohort_id`: cohort/group identifier, e.g. `C001`
- `biospecimen_id`: assay-linked provenance identifier, e.g. `B001`
- `assay_id`: molecular/cellular assay identifier, e.g. `A001`
- `dataset_id`: repository dataset identifier, e.g. `D001`
- `project_id`: project/consortium/research-program identifier, e.g. `PRJ001`
- `project_study_id`: explicit project-study relationship identifier, e.g. `PS001`
- `project_company_id`: explicit project-company relationship identifier, e.g. `PC001`
- `candidate_id`: unreviewed discovery identifier

IDs should never be recycled after public release.

## Source of truth
The files in `data/*.csv` are canonical. Excel files under `import_archive/` are historical/import snapshots only.

## Promotion rule
Automated discovery may create/update candidate records. Promotion into curated Study/Cohort/Biospecimen/Assay/Dataset, Project/Program or organization relationships is a human curation action.


## Companies as a translational discovery entity

`companies.csv` is intentionally broader than the evidence graph. A company can be curated because it directly uses menstrual fluid, menstrual blood or menstrual-derived material in a product, platform, research program or translational application even when no MFOX paper/project relationship has yet been verified.

Discovery metadata:
- `utilization_domain`
- `material_exploited`
- `maturity_level`
- `evidence_status`

These fields support discovery and filtering. They do not create a Study or Project relationship. Only `project_companies.csv` creates a verified Project / Program ↔ Company edge.
