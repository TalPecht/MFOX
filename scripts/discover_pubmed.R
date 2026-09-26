# Candidate discovery only: this script must never modify curated tables.
# It requires the CRAN package 'rentrez'.
#
# Run manually first. Once the query is stable, GitHub Actions can run it weekly.

if (!requireNamespace("rentrez", quietly=TRUE)) {
  stop("Install 'rentrez' before running discovery: install.packages('rentrez')")
}
library(readr)
library(dplyr)
`%||%` <- function(x,y) if (is.null(x) || length(x)==0) y else x

query <- paste0(
  '("menstrual fluid"[Title/Abstract] OR "menstrual blood"[Title/Abstract] ',
  'OR "menstrual effluent"[Title/Abstract] OR MenSC[Title/Abstract]) AND ',
  '(transcriptom*[Title/Abstract] OR RNA-seq[Title/Abstract] OR "single cell"[Title/Abstract] ',
  'OR proteom*[Title/Abstract] OR metabolom*[Title/Abstract] OR lipidom*[Title/Abstract] ',
  'OR methyl*[Title/Abstract] OR epigen*[Title/Abstract] OR microbiom*[Title/Abstract])'
)

res <- rentrez::entrez_search(db="pubmed", term=query, retmax=200, sort="pub date")
if (!length(res$ids)) quit(status=0)
summ <- rentrez::entrez_summary(db="pubmed", id=res$ids)

existing <- read_csv("data/studies.csv", show_col_types=FALSE)
candidates <- read_csv("data/candidates.csv", show_col_types=FALSE)
existing_pmids <- as.character(existing$pmid)

new_ids <- setdiff(as.character(res$ids), existing_pmids)
if (!length(new_ids)) {
  cat("No new PubMed candidates.\n")
  quit(status=0)
}

new_summ <- rentrez::entrez_summary(db="pubmed", id=new_ids)
rows <- lapply(seq_along(new_ids), function(i) {
  s <- new_summ[[i]]
  data.frame(
    candidate_id = paste0("AUTO-PMID-", new_ids[i]),
    detected_date = as.character(Sys.Date()),
    source = "PubMed automated discovery",
    title = s$title `%||%` "",
    year = substr(s$pubdate `%||%` "",1,4),
    doi_or_accession = new_ids[i],
    url = paste0("https://pubmed.ncbi.nlm.nih.gov/",new_ids[i],"/"),
    matched_terms = "automated MFOX query",
    candidate_type = "Publication",
    triage_status = "NEW",
    reviewer = "",
    decision_reason = "",
    stringsAsFactors=FALSE
  )
})
new <- bind_rows(rows)
all <- bind_rows(candidates,new) |> distinct(candidate_id,.keep_all=TRUE)
write_csv(all,"data/candidates.csv",na="")
cat("Added",nrow(new),"candidate records.\n")
