# MFOX Community v0.7

## Purpose
MFOX Community is broader than MFOX Evidence. Evidence retains the strict omics inclusion framework; Community captures the publication-linked human menstrual-fluid / menstrual-blood / menstrual-effluent research community.

## Authorship model
All captured authors are stored in `people.csv` and `authorships.csv`. A researcher is visible by default when at least one is true:
- first author on an eligible MF publication;
- last author;
- corresponding author;
- explicitly stated equal-contribution author; or
- contributor to more than one eligible MF publication.

Equal contribution is never inferred from order or symbols alone. It is set to Yes only when an explicit contribution statement is verified.

## Current seed and completeness
v0.7 establishes the schema and interface and seeds it from the current MFOX evidence bibliography plus several broader menstrual-fluid reviews/commentaries and a fully verified authorship record for Warren et al. 2018. It is **not yet an exhaustive bibliography of every menstrual-fluid publication**. Full-author capture is complete only where publication-level authorship was explicitly verified; other MFOX records currently contain at least the first author plus previously curated linked investigators.

The intended next curation pass is a systematic broader MF bibliography search, after which every author of every eligible publication can be imported while the visibility rule is computed automatically.
