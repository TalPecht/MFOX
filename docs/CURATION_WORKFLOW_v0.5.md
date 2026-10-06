# MFOX v0.5 living curation workflow

## Weekly automation
GitHub Actions runs `.github/workflows/discover.yml` every Monday at 06:00 UTC and can also be started manually from Actions > Discover MFOX candidates > Run workflow.

The workflow searches PubMed, compares PMIDs with curated studies and previously seen automated candidates, enriches new candidates with bibliographic metadata/abstract where available, writes only to `data/candidates.csv`, validates the database, and opens/updates a pull request.

Automated discovery NEVER promotes a record into the curated evidence tables.

## Human review in the app
Open the **Curator** tab. Select one candidate and inspect the paper link and metadata.

- **Include in MFOX** opens a structured curation form. `Validate & add to MFOX` generates linked Study, Cohort, Assay and Biospecimen IDs; optionally creates a Dataset record; marks the candidate INCLUDED; and runs the relational validator.
- **Exclude** requires a reason, writes the decision to `exclusions.csv`, and marks the candidate EXCLUDED.
- **Defer** records why the paper needs more checking and keeps it outside curated evidence.

A timestamped backup of the CSV tables is made under `data/backups/` before curation writes.

## Important deployment note
The curation interface writes to local CSV files. This is ideal for local RStudio curation followed by Git commit/push. On many hosted Shiny services the filesystem is ephemeral, so do not use the deployed public app as the authoritative write interface unless persistent storage/GitHub write-back is configured. Public deployments should also hide or authenticate the Curator tab.

## Recommended weekly routine
1. GitHub creates the automated candidate PR.
2. Review/merge the PR so `candidates.csv` reaches your local main branch.
3. `git pull` locally.
4. Run MFOX and use **Curator** to Include / Exclude / Defer.
5. Run `source("scripts/validate_database.R")`.
6. Commit and push the curated CSV changes.
