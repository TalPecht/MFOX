# MFOX v1.4.19 — Structured Community Profiles + Registry Expansion

- **Company maturity is shown directly in the Company Directory** as a prominent development-stage badge on every curated company card; evidence status remains visible on the same card.
- **Join the Community is now structured for both academia and industry**: profile type, sector, role, organization, city/region/country, website/profile/ORCID, research interests, clinical contexts, sample types, technologies, expertise, collaboration interests, and company-specific utilization/material/technology/application/maturity/evidence fields.
- Community submissions are stored in `data/community_submissions.csv` and appear privately in **MFOX Den → Community profiles** for curator review before promotion into public tables.
- Explore → Registered studies now has **22 curated registry records from six registry sources**, including an additional verified ChiCTR menstrual-blood-derived stem-cell trial.
- Registry search scope is explicitly tracked across ClinicalTrials.gov, ISRCTN, DRKS, the Netherlands registry, ChiCTR, ReBEC, CTIS, ANZCTR, CTRI, CRiS, JPRN/UMIN, PACTR, IRCT, TCTR, SLCTR and WHO ICTRP.
- Registry inclusion remains specimen-focused: menstruation symptom or blood-loss trials are not added unless menstrual material itself is collected, analyzed, or used as a derived therapeutic product.

# MFOX v1.4.18 — Simplified evidence flow

- Explore → Connections → Evidence flow now shows exactly three visible layers: **Sample type → Technology → Resource**.
- The intermediate **Curated studies** column was removed. Study/assay relationships are still used internally to verify which papers, datasets, projects/programs, institutions and companies connect to each technology.
- No scientific data tables were changed.

# MFOX v1.4.17 — Voting type fix

Focused bugfix for New & Unreviewed voting. Vote-log CSV columns are now read with explicit character types, preventing empty/header-only candidate_votes.csv files from being inferred as logical columns and breaking the first vote. A defensive character cast is also applied before every append/update. Scientific content is unchanged.

# MFOX-Menstrual Fluid Omics eXplorer — v1.4.16



## v1.4.16 — voting rebuilt from scratch

This release changes only the New & Unreviewed voting mechanism. The previous Shiny-bound action buttons and reactive per-candidate vote outputs have been removed. Vote controls are now plain HTML `type="button"` elements. A click sends one `candidate_vote_event` to the server, the server catches all vote errors and sends one direct `candidate_vote_ack` message back, and JavaScript updates only the clicked card and the four summary numbers. No public voting UI is invalidated or re-rendered by a vote.

Votes update immediately for the active session. Persistence to `data/candidate_votes.csv` remains best-effort; on a read-only deployment the UI explicitly says that the vote is session-only rather than failing silently. The voting session ID is generated internally and no longer depends on `session$token`.

## v1.4.15 — stable voting + multi-registry discovery

- Explore now opens on **Connections**, so users see the evidence graph before drilling into individual records.
- Registered studies were expanded from a small ClinicalTrials.gov seed into a **multi-registry curated layer**.
- Current registry sources include **ClinicalTrials.gov, ISRCTN, the German Clinical Trials Register (DRKS), and Onderzoek met mensen / ToetsingOnline (Netherlands)**.
- Registry discovery uses multiple specimen terms (`menstrual effluent`, `menstrual blood`, `menstrual fluid`, `menstruum`) plus registry identifiers found in papers/protocols; every included record is manually checked for actual menstrual-fluid collection or analysis.
- The registry view now has a source filter and can show cross-registration when known.
- Community voting was rebuilt with **native Shiny action buttons**. Candidate cards stay mounted; only the tiny vote-status line updates, preventing the full New & Unreviewed page from going grey or disappearing during a vote.

## v1.4.14 — registry studies + community/company consolidation

- Adds **Explore → Registered studies** with six seeded ClinicalTrials.gov menstrual-fluid-relevant registry records, kept explicitly separate from publication-derived assay evidence.
- Rebuilds **Companies → Utilization explorer** as Material → Company → Utilization flow plus a material/use heatmap.
- Removes the redundant Evidence-source link from public company cards when Website is the primary destination.
- Makes **Collaboration groups** the opening Community view, followed by Researchers & labs, Companies & innovators, Geography and Join the Community.
- Clarifies Community companies (WHO / WHERE) versus the top-level Companies utilization landscape (WHAT / HOW).
- Adds companies to Community Geography with a distinct marker color.
- Fixes New & Unreviewed voting so the interface does not fade and votes still register in-session on read-only deployments.

See `docs/V1.4.14_REGISTRY_COMMUNITY_COMPANIES.md` and `docs/QA_v1.4.14.txt`.

## v1.4.13 — clearer entry overviews and consolidated discovery

- Moves **Scientific connections** from Community into **Explore → Connections**, where it follows the same Explore evidence filters.
- Consolidates relationship visuals in one Explore tab: evidence flow plus scientific co-occurrence.
- Removes the separate **Companies → Evidence & maturity** tab; evidence and maturity summaries now appear directly in **Company directory**, and each company card continues to show both fields.
- Adds a consistent compact **Purpose / Inside / Start here** overview to the major working areas so users can understand a section before interacting with it.
- Keeps one-line **IN THIS VIEW** orientation strips at sub-tab level.
- Simplifies Community to people, organizations, collaboration groups and geography; scientific evidence relationships now live in Explore.

See `docs/V1.4.13_NAV_OVERVIEW.md` and `docs/QA_v1.4.13.txt`.


## v1.4.11 — functional + concise navigation pass

- Fixed candidate voting with native delegated browser events and explicit save success/error feedback.
- Rebuilt Companies → Evidence & maturity with robust missing-value handling, a category summary, and clearer separation of development stage from evidence status.
- Corrected the Landscape timeline: background bars are true unique yearly totals; colored category lines can overlap when one study/assay belongs to multiple categories.
- Made Explore more concise by removing the duplicated Explore-vs-Planner cards and moving relationship counts into Studies & assays.
- Renamed Explore → Evidence to Explore → Studies & assays.
- Added compact orientation strips to Explore, Companies, Community, and MFOX Den sub-tabs.
- Clarified Landscape (pattern-level view) versus Explore (record-level connected evidence).

See `docs/V1.4.11_FUNCTIONAL_CONCISE.md` and `docs/QA_v1.4.11.txt`.

## v1.4.8 — Readability + Community orientation

- Added a Community at-a-glance entry row with direct navigation to researchers/labs, companies, collaboration groups, scientific connections and geography.
- Increased typography throughout filters, tables, tabs, helper text, cards and navigation for easier reading.
- Enlarged the MFOX logo on both the Home hero and desktop sidebar.
- Preserved the v1.4.6/v1.4.7 data-first evidence canvas and scientific data model.

## v1.4.7 — Scientific editorial style refresh

Built on v1.4.6. This is a visual-system release: larger sidebar branding, clearer side-navigation states, calmer scientific cards/tables/tabs, and a dedicated Fox Den identity. Fox Den now explains its restricted curator role through Review → Structure → Maintain, using a new `www/mfox_den.svg` illustration. No scientific records or evidence relationships were changed.

See `docs/V1.4.7_VISUAL_STYLE.md` and `docs/QA_v1.4.7.txt`.


MFOX is a living knowledgebase and interactive explorer of the omics, molecular and cellular profiling landscape of human menstrual fluid and its derivatives.

## v1.4.6 — Data-first layout fix

This release fixes the v1.4.5 desktop side-navigation overlap and reallocates screen space toward evidence, tables and visualizations. The fixed navigation now offsets the actual bslib page container rather than the body. Explore and Companies no longer nest a second sidebar inside the app navigation; their filters are compact horizontal multi-column bars above a full-width evidence canvas.

Non-data page content is compressed: page intros, purpose cards, helper text, metric strips and planner controls take substantially less vertical space. Evidence canvases are larger (Explore connection map 520 px, Landscape 820 px, Community network 760 px, geography map 720 px). `page_navbar(fillable = FALSE)` is used so evidence-heavy pages scroll naturally instead of shrinking outputs into the remaining viewport.

All v1.4.5 scientific data and functionality are preserved. See `docs/V1.4.6_DATA_FIRST_LAYOUT.md` and `docs/QA_v1.4.6.txt`.

## v1.4.3 — Explore relational-link fix

Fixes the Explore runtime failure seen when filtering to a context such as **Endometriosis** with Technology = Any and Sample type = Any. Project/program relationship tables and project entities both contain `source_url`; previous joins let dplyr suffix these fields and downstream code still referenced `source_url`. v1.4.3 gives relationship and entity URLs explicit names before joins and adds a safe empty-state when a filtered evidence subset has no verified project/program relationship.

For Endometriosis, the curated database currently resolves to 11 studies, 14 assays, 11 mapped publications and 5 indexed datasets. No verified Project/Program relationship is currently stored for that filtered subset, so the Projects & Programs tab now reports that state rather than throwing an error.

## v1.0
The public interface now starts with the biology: menstrual fluid → sample fractions → measurement layers → evidence → community → study planning. Landscape visualisations share one evidence engine, Community avoids a dense all-author network, and Plan a Study includes exact/adjacent evidence, methods, experts and reusable data. See `docs/V1.0_RELEASE.md`.

## Run
```r
source("scripts/validate_database.R")
shiny::runApp()
```

# MFOX — Menstrual Fluid Omics Explorer

MFOX is a living, curated knowledgebase and interactive explorer of the menstrual-fluid omics landscape.

## Scientific model

The **CSV files in `data/` are the canonical database**. The Shiny application is a view over those files.

Core evidence relationship:

`Study → Cohort → Biospecimen → Assay → Dataset`

Connected knowledge layer:

`Project / Program ↔ Study ↔ Paper ↔ Dataset` and `Study ↔ Researcher ↔ Institution`, with explicit verified `Project / Program ↔ Company` links.

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

- **Home** — field snapshot and navigation
- **Landscape** — visual evidence composition and timelines
- **Explore** — discovery mode for curated studies/assays and their linked papers, datasets, projects, labs and companies
- **Companies** — menstrual-fluid utilization and translational discovery landscape
- **Community** — researchers, institutions, scientific connections and geography
- **Plan a Study** — targeted design intelligence for a proposed study definition
- **Contribute** — community submissions for curator review
- **New & Unreviewed** — candidate queue
- **Fox Den** — curator workflows and structured database entry

## Curation rule

Automated discovery may update **only `data/candidates.csv`**. It must never promote a record directly into curated tables. Human review is required.

## Current status

This is a v0.1 GitHub-ready prototype seeded from the current MFOX relational workbook. It is not yet a complete systematic review database.

See `docs/curation_protocol.md` and `docs/data_dictionary.md`.

## v0.5 — living curation
MFOX v0.5 adds weekly PubMed candidate discovery and a local **Curator** interface for Include / Exclude / Defer decisions. See `docs/CURATION_WORKFLOW_v0.5.md`.


## v0.6 — Community
Adds a curated investigator/affiliation layer and interactive global map for field navigation and collaborator discovery. See `docs/PEOPLE_LABS_v0.6.md`.


## Community v0.7
See `docs/COMMUNITY_v0.7.md` for the publication/authorship eligibility model and current completeness statement.

## v0.8 scientific scope update
MFOX now maps omics, molecular **and cellular profiling** of menstrual fluid. Flow cytometry is an explicit assay modality. Microbiome is a research domain; 16S rRNA sequencing and metatranscriptomics are assay modalities. See `docs/V0.8_RELEASE.md`.

## v0.8.1 Maps & Networks
Community now includes an institution-centred geographic explorer with rich scientific popups and a Scientific Network view for co-authorship and technology/sample/disease/domain relationships. Missing affiliation coordinates are reported explicitly rather than silently omitted.

## v1.1.1 update
Adds a normalized clinical-context ontology, a modular Study Planner workspace, and a curated Companies & Translation tab. The company table is an initial verified seed rather than an exhaustive commercial landscape.

## v1.1.5 contribution workflow
MFOX now includes a public-facing Contribute tab. Community submissions are staged in `data/contributions.csv` and must be curator-reviewed before promotion into the scientific source tables. Public deployments should connect this form to persistent storage because some Shiny hosting environments use ephemeral filesystems.

## v1.1.7 — Fox Den curator prototype

MFOX now includes a protected **Fox Den** review workspace for community contributions. Configure a local/deployment curator password before launching:

```r
Sys.setenv(MFOX_FOX_PASSWORD = "choose-a-strong-local-password")
shiny::runApp()
```

Foxes can review the original submission, assign preliminary canonical sample/clinical-context/technology metadata, and choose **Accept to curation queue**, **Needs information**, or **Out of scope**. Every decision is appended to `data/curation_history.csv`. Accepted suggestions are promoted to `data/candidates.csv` with source `Community submission` and status `Fox-approved — needs structured curation`; they do **not** bypass the linked Study → Cohort → Biospecimen → Assay → Dataset curation step.

The shared environment-variable password is a prototype for local testing only. A public deployment should use individual authenticated curator accounts so reviewer identity, access revocation, and the audit trail are reliable.

## v1.3.3 naming update

The canonical project title throughout the interface and release metadata is **MFOX-Menstrual Fluid Omics eXplorer**. This release retains the v1.1.7 Fox Den, Contribute workflow, company expansion, planner/workspace, assay transparency, ontology cleanup, and navigation/plot fixes.

## v1.3.3 — Fox multi-classification + contributor notifications
- Fox Den canonical sample, clinical context and assay/technology are searchable multi-select controls.
- Review history stores multiple controlled values as semicolon-delimited relationships for the review/audit layer; structured promotion remains a curator step.
- Contribute now offers an optional email field plus an explicit, unchecked decision-notification opt-in.
- Contributor email stays only in the private contributions queue and is not copied into public evidence tables.
- Decision emails are sent when `MFOX_RESEND_API_KEY` and `MFOX_FROM_EMAIL` are configured. If mail is not configured or delivery fails, the record is retained with a queued notification status rather than losing the decision.


## v1.3.3 Fox Den fix
Restores the curator review workflow, keeps searchable multi-select fields for canonical sample, clinical context and assay/technology, and stores each selected value as a separate structured curation relationship. The Fox Den hero has also been redesigned as a distinct den-like curation space.


## v1.3.3 usability fixes
Fixes Contribute schema typing for notification opt-in, replaces the Study Planner expert table with institution/publication-grouped expert cards with external profile and paper links, and replaces the temporary geometric Fox Den mark with the MFOX logo presented inside a den/cave emblem.


## v1.3.3 workflow fixes
Adds evidence synthesis in place of the brittle adjacent-evidence table, makes expert groups institution-first with compact linked publications, hardens contribution schema typing including email notification fields, and moves Fox review into a large modal workspace that closes and refreshes after a decision.


## v1.3.3 Planner simplification
Study Planner is deliberately reduced to four reader-facing panels: Exact evidence, Reusable datasets, Relevant experts, and Companies. Evidence synthesis and Methods & precedent are removed. Relevant experts returns to a compact table and hides authorship-position columns; institution/country are shown when available and otherwise left as an em dash rather than inferred.


## v1.3.3 Fox Den modal fix
Fixes the grey-screen state after a curator decision by explicitly removing any stale Bootstrap modal backdrop/body lock after the review modal closes, while preserving queue refresh and the next-review workflow.


## v1.3.3 Den stability + expert paper links
Removes the Bootstrap modal from Fox Den review entirely to eliminate persistent grey-backdrop failures. Review is now an inline two-column workspace under the selected submission. In Study Planner, MF papers now contains direct publication links rather than a numeric count.


## v1.3.3 Fox Den crash fix
The persistent grey screen was treated as a Shiny session/server error rather than a modal problem. Curator decisions are now wrapped in failure-safe handling; legacy CSV types are normalized before row binding; candidate/history writes cannot silently crash the session; synchronous email/network calls are removed from the decision click; and the reactive queue is allowed to redraw without imperatively updating an input that is being destroyed/recreated. Email opt-ins are marked Pending notification for separate delivery.


## v1.3.3 Fox Den rebuild
Replaces the submission dropdown/reused form with a persistent master-detail review queue. Each submission gets a fresh set of dynamically keyed input IDs, so classifications cannot leak from the previous record. On decision the selected record is cleared before the reactive data refresh, and accepted/out-of-scope records therefore disappear from the pending queue immediately. This design uses no modal, overlay, notification-driven navigation, or imperative select update.


## v1.3.3 Fox Den stability rebuild
The Den now uses a static post-login shell with a DT review queue and a separate fixed review form. Decisions no longer invalidate/re-render the entire Den, eliminating the main source of persistent shaded/recalculating UI. Accepted and out-of-scope submissions are removed from the pending queue after save; review inputs are reset from the selected submission's own stored curation links, never from the previous record. The page uses natural browser scrolling instead of an inner fixed-height queue. Decision emails are queued independently of review saves and can be sent from the Den or by `scripts/send_pending_notifications.R`; this prevents mail/network failures from blocking curation. See `EMAIL_SETUP.md`.


## v1.3.3 — accepted-resource visibility and two-way voting
- Accepted Community submissions now have a dedicated Fox Den tab.
- Accepted publication submissions appear immediately in Community → Publications and in Explore under “Accepted publications awaiting structured evidence curation.” They do not enter the structured evidence matrix until Study → Cohort → Biospecimen → Assay mapping is completed.
- Community candidate voting now supports both Vote for and Vote against. Legacy votes are treated as positive votes. One session has one current vote per candidate and can switch direction.
- Fox Den automated discovery shows For, Against, and Balance explicitly; voting remains triage-only and never determines inclusion.


## v1.3.3 — curator direct evidence entry

Fox Den now includes **Add evidence**, a curator-only structured entry workflow that bypasses the public Contribute queue. It can:

- create a new Study → Cohort → Biospecimen → Assay relationship plus its Community publication;
- add a new sample/assay relationship to an existing study, using an existing or new cohort;
- optionally register a reusable dataset;
- capture the first author in the Community graph when supplied;
- write an audit row to `data/manual_evidence_log.csv`;
- create a timestamped backup of all touched CSVs in `data/curation_backups/` before replacing canonical files;
- reload the in-memory MFOX knowledgebase after a successful write so Explore/Landscape/Data can reflect the addition in the same running session.

The direct-entry form deliberately represents **one cohort–sample–assay relationship at a time**. This avoids falsely implying that every assay in a multi-assay paper was applied to every sample or cohort.


## v1.3.3 — Explore integration + direct review curation

- Timeline redesigned as annual stacked evidence composition with a total-evidence line, explicit zero baseline, cleaner grid and the same categorical palette as the Composition view.
- Reader-facing ambiguous sample label changed to **Mixed cells/tissue fraction — source did not separate**. Explore now shows the original/source sample wording and assay method alongside the canonical sample class.
- **Papers and Datasets are now inside Explore**. Paper rows link to reusable datasets from the same MFOX study; dataset rows link back to the represented publication(s). The former standalone Data tab and Community Publications tab are removed.
- Fox Den is the final top-level navigation tab.
- Accepted-but-not-yet-curated publication, dataset, company and researcher/lab submissions are visible in their intended public destination with an explicit pending status.
- Community review now has **Curate & add to database**. It transfers the selected submission into the curator-only Add evidence workflow, prefills available metadata, and supports four structured destinations: Publication + evidence, Dataset, Company, and Researcher/lab.
- When a directly curated submission is successfully written, its contribution status becomes **Accepted — curated into database**, it leaves the review queue, its audit history is updated, and opted-in email notification remains queued for the separate email worker.


## v1.4.0 — connected menstrual-fluid knowledgebase

MFOX now treats projects/programs as first-class curated entities and makes **Explore** the central connected discovery surface.

- Added canonical `projects.csv`, `project_studies.csv`, and `project_companies.csv`.
- Explore now contains Evidence, Papers, Datasets, Projects & programs, and Labs & companies.
- Papers and datasets show verified project/program relationships from the same MFOX study.
- Labs/institutions are derived from publication authorship and follow Explore filters.
- Companies remain a separate translational layer and are shown as evidence-connected only when an explicit project/program relationship exists.
- Accepted project/program submissions are visible before structured relationship curation, with an explicit pending state.
- Fox Den can directly create a project/program and optionally attach verified studies and companies; no relationship is inferred automatically.
- The former standalone Companies top-level tab is consolidated into Explore; company evidence remains available in Study Planner.
- v1.4.0 seeds two project/program entities from verified public sources: MultiMENDo and the NextGen Jane Menstrualome research program. MultiMENDo is intentionally not linked to a specific MFOX paper until that relationship is curator-verified.


## v1.4.1 — Companies discovery landscape

Companies is restored as a top-level discovery surface while remaining available as an evidence-linked entity inside Explore.

The two views intentionally answer different questions:

- **Explore → Labs & companies**: which companies have an explicit curated relationship to the currently filtered evidence?
- **Companies**: what translational and commercial uses of menstrual fluid are represented in the curated landscape, including emerging companies that do not yet have a linked MFOX publication/project?

`data/companies.csv` now includes four normalized discovery fields:

- `utilization_domain` — semicolon-delimited use areas such as diagnostics, longitudinal monitoring, collection/preanalytics, research infrastructure, regenerative medicine and cell-derived products.
- `material_exploited` — semicolon-delimited menstrual-fluid material/component classes.
- `maturity_level` — a descriptive development-stage label.
- `evidence_status` — the strongest type of public support currently curated by MFOX. This is descriptive and is not a quality score.

Fox Den company curation captures these fields for new records. Company discovery never requires a paper link; explicit Project ↔ Company and Study ↔ Project links continue to govern evidence-linked filtering in Explore.


## v1.4.2 — Explore guidance + targeted Study Planner

This release separates two previously overlapping workflows:

- **Explore = discovery mode.** Browse what already exists in the MFOX evidence base, filter by clinical context, technology and sample, and follow the same evidence to papers, datasets, projects, labs and evidence-linked companies.
- **Plan a Study = planning mode.** Define at least one feature of a proposed study and retrieve targeted design precedent: study designs, collection/processing details, assays, datasets, researchers and translational links. The planner no longer defaults to reproducing the entire Explore evidence table when every field is `Any`.

Explore now exposes an explicit **Any** choice for Clinical context, Technology and Sample type. `Any` is neutral; if a specific value is selected in the same dimension, the specific value controls filtering. Reset returns all three dimensions to `Any`.

The **Curated studies and assays** card now contains a large start guide when no specific Explore filters are active, an explicit no-match guide when a combination returns zero curated evidence, and a compact result summary for active matches.


## v1.4.4 — Community companies + planner overlap

Community now includes a **Companies & innovators** view so industry and technology organizations appear alongside researchers, labs and institutions. Plan a Study now suggests curated companies when their disease/application, technology family, menstrual-fluid material, or longitudinal-use profile overlaps the proposed study, and each card states the exact overlap dimensions. These matches indicate translational relevance, not endorsement or equivalent evidence strength.
