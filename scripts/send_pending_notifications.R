# MFOX queued email worker
# Run with: Rscript scripts/send_pending_notifications.R
# Required environment variables:
#   MFOX_RESEND_API_KEY
#   MFOX_FROM_EMAIL

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})
if(!requireNamespace("curl",quietly=TRUE)) stop("Package 'curl' is required.")
if(!requireNamespace("jsonlite",quietly=TRUE)) stop("Package 'jsonlite' is required.")

key <- Sys.getenv("MFOX_RESEND_API_KEY",unset="")
from <- Sys.getenv("MFOX_FROM_EMAIL",unset="")
if(!nzchar(key) || !nzchar(from)) stop("Set MFOX_RESEND_API_KEY and MFOX_FROM_EMAIL before running the worker.")

path <- file.path("data","contributions.csv")
if(!file.exists(path)) stop("data/contributions.csv not found. Run this script from the MFOX app directory.")
d <- read_csv(path,show_col_types=FALSE,na=c("","NA"))
for(nm in c("submitter_email","notification_status","notified_at","curator_notes","status")){if(!nm%in%names(d))d[[nm]]<-NA_character_;d[[nm]]<-as.character(d[[nm]])}
if(!"notification_opt_in"%in%names(d))d$notification_opt_in<-FALSE
opt <- tolower(trimws(as.character(d$notification_opt_in))) %in% c("true","t","1","yes","y")
eligible <- opt & !is.na(d$submitter_email) & nzchar(d$submitter_email) & !d$status %in% c("Submitted","Under review") & (is.na(d$notification_status)|!d$notification_status%in%c("Sent","Not requested"))
ids <- which(eligible)
if(!length(ids)){cat("No pending MFOX decision emails.\n");quit(save="no",status=0)}

for(i in ids){
  subject <- paste0("MFOX submission ",d$submission_id[i]," — ",d$status[i])
  body <- paste0("Thank you for contributing to MFOX-Menstrual Fluid Omics eXplorer.\n\nSubmission: ",d$submission_id[i],"\nResource: ",d$title_or_name[i],"\nDecision: ",d$status[i],if(!is.na(d$curator_notes[i])&&nzchar(d$curator_notes[i]))paste0("\nCurator note: ",d$curator_notes[i])else"","\n\nThis is the decision notification you requested when submitting the resource.")
  result <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setheaders(h,"Authorization"=paste("Bearer",key),"Content-Type"="application/json")
    curl::handle_setopt(h,postfields=jsonlite::toJSON(list(from=from,to=list(d$submitter_email[i]),subject=subject,text=body),auto_unbox=TRUE),connecttimeout=5,timeout=12)
    res <- curl::curl_fetch_memory("https://api.resend.com/emails",handle=h)
    if(res$status_code>=200&&res$status_code<300) list(sent=TRUE,status="Sent") else list(sent=FALSE,status=paste0("Queued — Resend returned HTTP ",res$status_code))
  },error=function(e)list(sent=FALSE,status=paste0("Queued — ",conditionMessage(e))))
  d$notification_status[i] <- result$status
  if(isTRUE(result$sent)) d$notified_at[i] <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
  cat(d$submission_id[i],": ",result$status,"\n",sep="")
}
write_csv(d,path)
