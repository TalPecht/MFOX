# App structure

- `app.R` - Shiny UI/server entry point
- `R/data_load.R` - reads canonical CSV tables and builds joined evidence view
- `R/filters.R` - reusable filtering helpers
- `R/validation.R` - schema, uniqueness, and foreign-key checks
- `data/` - canonical MFOX knowledgebase
- `www/` - static app assets such as logo/CSS
- `scripts/` - validation, discovery, and release utilities
- `.github/workflows/` - automated validation and candidate discovery
- `docs/` - scientific and technical documentation
- `import_archive/` - non-canonical legacy/import material
