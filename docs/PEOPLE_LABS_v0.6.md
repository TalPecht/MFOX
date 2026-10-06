# MFOX People & Labs — v0.6

## Purpose
This module maps investigators represented in the curated MFOX evidence base using publication affiliations. It is a field-navigation and collaborator-discovery aid, not a ranking.

## Data model
`data/people_labs.csv` stores investigator name, role, linked MFOX study IDs, institution, city/country, map coordinates, expertise tags, ORCID/profile URL when available, and curation notes.

## Map interpretation
Coordinates are institution/city-level approximations for visualization. They are not personal locations. Multi-affiliated authors are represented by a primary/representative publication affiliation in this release.

## Curation
The initial v0.6 seed focuses on investigators/groups with clearly verifiable affiliations in the curated MFOX papers. Future releases should expand author coverage systematically from publication metadata.

## Interface
Users can filter by expertise and country, search investigator/institution text, restrict to direct menstrual-fluid studies, click map markers for group details, and open linked public publication/profile records.
