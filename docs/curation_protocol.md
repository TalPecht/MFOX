# MFOX curation protocol — draft

## Scope
Include human menstrual fluid / menstrual blood / menstrual effluent and biological materials derived from them when profiled using molecular or high-dimensional approaches.

## Primary omics scope
Transcriptomics, single-cell/single-nucleus sequencing, proteomics, metabolomics, lipidomics, epigenomics, microbiome profiling, high-dimensional cytometry and comparable multiplex assays.

## Provenance
Do not silently pool all menstrual-derived material. Record:
- biospecimen class
- derivation class
- ex-vivo manipulation
- culture status
- passage number when reported
- `direct_mf_scope`

## Candidate workflow
`NEW → SCREENING → NEEDS_FULL_TEXT → ELIGIBLE/EXCLUDED → CURATED`

Automated scripts write only to `candidates.csv`.

## Missingness
Use `NR` or an agreed controlled value for information that was not reported. Do not treat an empty field as equivalent to "No".

## Versioning
Freeze a database release for every manuscript analysis. The live app may continue to update after that release.
