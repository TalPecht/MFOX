# Release checklist

Before tagging a MFOX release:

- [ ] Run `Rscript scripts/validate_database.R`
- [ ] Review all newly curated IDs and foreign-key links
- [ ] Confirm candidate records are not counted in the curated landscape
- [ ] Confirm repository accessions/URLs
- [ ] Freeze search/curation cut-off date
- [ ] Update README version/date
- [ ] Test all Shiny tabs locally
- [ ] Tag release (e.g. `v0.1.0`)
- [ ] Archive manuscript releases with a DOI when appropriate
