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
candidates <- read_csv(
  "data/candidates.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)
known <- unique(c(as.character(existing$pmid), sub("AUTO-PMID-", "", candidates$candidate_id[grepl("^AUTO-PMID-", candidates$candidate_id)])))
new_ids <- setdiff(as.character(res$ids), known[!is.na(known) & known!=""])
if (!length(new_ids)) { cat("No new PubMed candidates.\n"); quit(status=0) }

summ <- rentrez::entrez_summary(
  db = "pubmed",
  id = new_ids,
  always_return_list = TRUE
)
get_id <- function(s, type) {
  ids <- s$articleids

  if (is.null(ids) || length(ids) == 0) return("")

  # PubMed summaries commonly return articleids as a data frame
  if (is.data.frame(ids)) {
    if (!all(c("idtype", "value") %in% names(ids))) return("")

    hit <- which(tolower(as.character(ids$idtype)) == tolower(type))

    if (!length(hit)) return("")

    value <- as.character(ids$value[hit[1]])

    if (is.na(value)) "" else value
  } else if (is.list(ids)) {

    # Fallback for alternative rentrez structures
    values <- vapply(ids, function(z) {

      if (!is.list(z)) return("")

      idtype <- z[["idtype"]] %||% ""
      value  <- z[["value"]] %||% ""

      if (tolower(as.character(idtype)) == tolower(type)) {
        as.character(value)
      } else {
        ""
      }

    }, character(1))

    values <- values[nzchar(values)]

    if (length(values)) values[1] else ""

  } else {
    ""
  }
}
get_authors <- function(s) {
  authors <- s$authors
  
  if (is.null(authors) || length(authors) == 0) return("")
  
  if (is.data.frame(authors)) {
    if ("name" %in% names(authors)) {
      return(paste(as.character(authors$name), collapse = "; "))
    }
    return("")
  }
  
  if (is.list(authors)) {
    names_out <- vapply(authors, function(a) {
      if (is.list(a) && !is.null(a[["name"]])) {
        as.character(a[["name"]])
      } else {
        ""
      }
    }, character(1))
    
    names_out <- names_out[nzchar(names_out)]
    
    return(paste(names_out, collapse = "; "))
  }
  
  ""
}
get_abs <- function(pmid) {
  tryCatch(rentrez::entrez_fetch(db="pubmed", id=pmid, rettype="abstract", retmode="text"), error=function(e) "")
}
rows <- lapply(seq_along(new_ids), function(i) {
  s <- summ[[i]]; pmid <- new_ids[i]; doi <- get_id(s,"doi")
  data.frame(
    candidate_id=paste0("AUTO-PMID-",pmid), detected_date=as.character(Sys.Date()), source="PubMed automated discovery",
    title=s$title %||% "", year=substr(s$pubdate %||% "",1,4), pmid=pmid, doi=doi,
    journal=s$fulljournalname %||% s$source %||% "", authors=get_authors(s),
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
