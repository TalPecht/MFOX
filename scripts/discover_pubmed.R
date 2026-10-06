# MFOX weekly PubMed candidate discovery.
# Discovery ONLY: never modifies curated studies/cohorts/assays/biospecimens/datasets.
if (!requireNamespace("rentrez", quietly=TRUE)) stop("Install rentrez")
library(readr); library(dplyr)
`%||%` <- function(x,y) if (is.null(x) || length(x)==0) y else x

query <- paste0(
  '("menstrual fluid"[Title/Abstract] OR "menstrual blood"[Title/Abstract] OR ',
  '"menstrual effluent"[Title/Abstract] OR "menstrual-derived"[Title/Abstract] OR MenSC[Title/Abstract]) AND ',
  '(transcriptom*[Title/Abstract] OR RNA-seq[Title/Abstract] OR sequencing[Title/Abstract] OR ',
  '"single cell"[Title/Abstract] OR "single-cell"[Title/Abstract] OR "single nucleus"[Title/Abstract] OR ',
  'proteom*[Title/Abstract] OR metabolom*[Title/Abstract] OR lipidom*[Title/Abstract] OR ',
  'methyl*[Title/Abstract] OR epigen*[Title/Abstract] OR microbiom*[Title/Abstract] OR ',
  'metagenom*[Title/Abstract] OR metatranscriptom*[Title/Abstract] OR "extracellular vesicle"[Title/Abstract])'
)

res <- rentrez::entrez_search(db="pubmed", term=query, retmax=500, sort="pub date")
if (!length(res$ids)) quit(status=0)
existing <- read_csv("data/studies.csv", show_col_types=FALSE)
candidates <- read_csv("data/candidates.csv", show_col_types=FALSE)
known <- unique(c(as.character(existing$pmid), sub("AUTO-PMID-", "", candidates$candidate_id[grepl("^AUTO-PMID-", candidates$candidate_id)])))
new_ids <- setdiff(as.character(res$ids), known[!is.na(known) & known!=""])
if (!length(new_ids)) { cat("No new PubMed candidates.\n"); quit(status=0) }

summ <- rentrez::entrez_summary(db="pubmed", id=new_ids)
get_id <- function(s, type) {
  ids <- s$articleids
  if (is.null(ids)) return("")
  hit <- vapply(ids, function(z) identical(tolower(z$idtype %||% ""), type), logical(1))
  if (!any(hit)) return("")
  ids[[which(hit)[1]]]$value %||% ""
}
get_abs <- function(pmid) {
  tryCatch(rentrez::entrez_fetch(db="pubmed", id=pmid, rettype="abstract", retmode="text"), error=function(e) "")
}
rows <- lapply(seq_along(new_ids), function(i) {
  s <- summ[[i]]; pmid <- new_ids[i]; doi <- get_id(s,"doi")
  data.frame(
    candidate_id=paste0("AUTO-PMID-",pmid), detected_date=as.character(Sys.Date()), source="PubMed automated discovery",
    title=s$title %||% "", year=substr(s$pubdate %||% "",1,4), pmid=pmid, doi=doi,
    journal=s$fulljournalname %||% s$source %||% "", authors=paste(vapply(s$authors %||% list(), function(a) a$name %||% "", character(1)), collapse="; "),
    abstract=get_abs(pmid), doi_or_accession=ifelse(doi!="",doi,pmid), url=paste0("https://pubmed.ncbi.nlm.nih.gov/",pmid,"/"),
    matched_terms="MFOX PubMed query v0.5", candidate_type="Publication", triage_status="NEW", reviewer="", decision_reason="",
    stringsAsFactors=FALSE
  )
})
new <- bind_rows(rows)
# Align old/new columns safely.
for (nm in setdiff(names(new), names(candidates))) candidates[[nm]] <- NA_character_
for (nm in setdiff(names(candidates), names(new))) new[[nm]] <- NA_character_
new <- new[,names(candidates)]
write_csv(bind_rows(candidates,new) |> distinct(candidate_id,.keep_all=TRUE), "data/candidates.csv", na="")
cat("Added",nrow(new),"new PubMed candidate(s).\n")
