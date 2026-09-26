# MFOX — Menstrual Fluid Omics Explorer

MFOX is a living, curated knowledgebase and interactive explorer of the menstrual-fluid omics landscape.

## Scientific model

The **CSV files in `data/` are the canonical database**. The Shiny application is a view over those files.

Core relationship:

`Study → Cohort → Biospecimen → Assay → Dataset`

MFOX includes whole menstrual fluid and menstrual-fluid-derived materials. Provenance is explicit, so direct menstrual-fluid profiling can be separated from cultured or experimentally manipulated derivatives.

## Run locally

1. Install R and RStudio.
2. Open this repository as an RStudio project/folder.
3. Install packages:

```r
install.packages(c("shiny","bslib","dplyr","tidyr","readr","stringr","DT","ggplot2","plotly"))
```

4. Run:

```r
shiny::runApp()
```

## App sections

- **Landscape** — clinical context × omics evidence map
- **Explore** — filter studies and assays
- **Gaps** — evidence-density matrices
- **Plan a Study** — retrieve closely related MFOX records
- **Data** — public datasets/accessions
- **New & Unreviewed** — candidate queue

## Curation rule

Automated discovery may update **only `data/candidates.csv`**. It must never promote a record directly into curated tables. Human review is required.

## Current status

This is a v0.1 GitHub-ready prototype seeded from the current MFOX relational workbook. It is not yet a complete systematic review database.

See `docs/curation_protocol.md` and `docs/data_dictionary.md`.
