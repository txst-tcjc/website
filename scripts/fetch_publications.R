# fetch_publications.R
# Builds data/publications.yml from each center member's ORCID record.
#
#   Run from the website/ folder:  Rscript scripts/fetch_publications.R
#   Then rebuild the site:         quarto render
#
# Sources: ORCID (which works belong to whom) + Crossref (authors, journal, date, by DOI).
# Do not hand-edit data/publications.yml; it is overwritten on every run.
# Hand-curated info lives in data/publication_tags.yml (topic tags, hiding items),
# which this script reads and merges in, so curation survives re-runs.

library(yaml)
library(jsonlite)

ua <- "TCJC-website-publications/1.0"   # generic client name, no personal info

get_json <- function(url, accept = "application/json") {
  con <- url(url, headers = c(Accept = accept, `User-Agent` = ua))
  on.exit(try(close(con), silent = TRUE))
  tryCatch(fromJSON(readLines(con, warn = FALSE), simplifyVector = FALSE), error = function(e) NULL)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a
# Crossref sometimes returns HTML entities (e.g. "&amp;"); decode the common ones
unent <- function(x) { x <- gsub("&amp;", "&", x, fixed = TRUE); x <- gsub("&lt;", "<", x, fixed = TRUE); gsub("&gt;", ">", x, fixed = TRUE) }
first <- function(x) if (length(x)) x[[1]] else NULL   # first element, or NULL if empty list
norm_title <- function(x) gsub("[^a-z0-9]", "", tolower(x))
short_name <- function(name) trimws(sub(",.*$", "", name))

# ---- 1. Center members with an ORCID iD ------------------------------------
people <- Filter(function(p) nzchar(p$orcid %||% ""), read_yaml("data/people.yml"))
if (!length(people)) stop("No one in data/people.yml has an orcid: value.")

# ---- 2. Their ORCID works (one summary per ORCID work group) ---------------
works <- list()
for (p in people) {
  message("ORCID: ", p$name)
  d <- get_json(sprintf("https://pub.orcid.org/v3.0/%s/works", p$orcid))
  for (g in d$group %||% list()) {
    s   <- g$`work-summary`[[1]]
    ids <- s$`external-ids`$`external-id` %||% list()
    doi <- Filter(function(e) e$`external-id-type` == "doi", ids)
    works[[length(works) + 1]] <- list(
      person = short_name(p$name),
      title  = s$title$title$value,
      year   = s$`publication-date`$year$value %||% NA,
      doi    = if (length(doi)) tolower(doi[[1]]$`external-id-value`) else NA,
      source = s$`journal-title`$value %||% "",
      otype  = s$type
    )
  }
}

# ---- 3. Crossref details by DOI ----------------------------------------------
apa_authors <- function(au) {
  nm <- vapply(au, function(a) {
    if (is.null(a$family)) return(a$name %||% "")
    ini <- paste0(substr(strsplit(a$given %||% "", "[ .-]+")[[1]], 1, 1), ".", collapse = " ")
    if (ini == ".") a$family else paste0(a$family, ", ", ini)
  }, "")
  nm <- nm[nzchar(nm)]
  if (length(nm) <= 1) return(nm)
  paste0(paste(nm[-length(nm)], collapse = ", "), ", & ", nm[length(nm)])
}
for (i in seq_along(works)) {
  w <- works[[i]]
  if (is.na(w$doi)) next
  cr <- get_json(paste0("https://api.crossref.org/works/", utils::URLencode(w$doi, reserved = TRUE)))$message
  if (is.null(cr)) next
  dp <- (cr$`published-print` %||% cr$`published-online` %||% cr$issued)$`date-parts`[[1]]
  works[[i]]$authors <- apa_authors(cr$author %||% list())
  if (!length(works[[i]]$authors) || !nzchar(works[[i]]$authors[1])) {        # edited books: use editors
    ed <- cr$editor %||% list()
    if (length(ed)) works[[i]]$authors <- paste0(apa_authors(ed), if (length(ed) > 1) " (Eds.)" else " (Ed.)")
  }
  works[[i]]$source  <- unent(first(cr$`container-title`) %||% (w$source %||% (first(cr$publisher) %||% cr$publisher %||% "")))
  works[[i]]$year    <- dp[[1]] %||% w$year
  works[[i]]$date    <- if (length(dp) >= 2) sprintf("%04d-%02d-%02d", dp[[1]], dp[[2]], if (length(dp) >= 3) dp[[3]] else 1L) else ""   # day defaults to 1
  works[[i]]$ctype   <- cr$type %||% ""
  works[[i]]$title   <- unent(gsub("<[^>]+>", "", first(cr$title) %||% w$title))   # strip Crossref markup
  Sys.sleep(0.2)                                                          # be polite to the API
}

# ---- 3b. Publication type -----------------------------------------------------
# Rules (first match wins), using the outlet name plus Crossref/ORCID type:
#   CrimRxiv / OSF / *Rxiv outlets or Crossref "posted-content" -> Preprint
#   Crossref/ORCID book-chapter                                  -> Book chapter
#   Crossref/ORCID book, edited-book, monograph                  -> Book
#   outlet named "...Briefing(s)" / "...Brief(s)"                -> Brief
#   journal-article                                              -> Journal article
#   anything else                                                -> Report
# A "type:" entry in data/publication_tags.yml overrides the rule for one item.
pub_type <- function(w) {
  src <- tolower(w$source %||% ""); t <- tolower(paste(w$ctype %||% "", w$otype %||% ""))
  if (grepl("rxiv|osf", src) || grepl("posted-content|preprint", t)) "Preprint"
  else if (grepl("book-chapter", t)) "Book chapter"
  else if (grepl("book|monograph", t)) "Book"
  else if (grepl("brief", src)) "Brief"
  else if (grepl("journal-article", t)) "Journal article"
  else "Report"
}
for (i in seq_along(works)) works[[i]]$type <- pub_type(works[[i]])

# ---- 4. De-duplicate (same title from preprint + journal, or co-authored by two fellows)
# Preference: journal article > book chapter / report > preprint
rank <- function(w) match(w$type, c("Journal article", "Book", "Book chapter", "Brief", "Report", "Preprint"))
key  <- vapply(works, function(w) norm_title(w$title), "")
pubs <- list()
for (k in unique(key)) {
  grp  <- works[key == k]
  best <- grp[[which.min(vapply(grp, rank, 0))]]
  best$people <- sort(unique(vapply(grp, `[[`, "", "person")))
  pubs[[length(pubs) + 1]] <- best
}

# ---- 5. Merge hand-curated tags / hides --------------------------------------
cur <- if (file.exists("data/publication_tags.yml")) read_yaml("data/publication_tags.yml") %||% list() else list()
cur_by <- setNames(cur, vapply(cur, function(x) tolower(x$doi %||% norm_title(x$title %||% "")), ""))
out <- list()
for (w in pubs) {
  c <- cur_by[[tolower(w$doi %||% "")]] %||% cur_by[[norm_title(w$title)]]
  if (isTRUE(c$hide)) next
  out[[length(out) + 1]] <- list(
    year    = as.integer(w$year),
    date    = w$date %||% "",
    title   = w$title,
    authors = w$authors %||% "",
    source  = w$source %||% "",
    type    = c$type %||% w$type,
    url     = if (!is.na(w$doi)) paste0("https://doi.org/", w$doi) else "",
    topics  = as.list(unlist(c$topics) %||% character(0)),
    people  = as.list(w$people)
  )
}
untagged <- sum(vapply(out, function(x) !length(x$topics), TRUE))

header <- paste0(
  "# publications.yml: GENERATED by scripts/fetch_publications.R on ", Sys.Date(), ".\n",
  "# Do not edit by hand; changes are overwritten. Add topic tags or hide items in\n",
  "# data/publication_tags.yml, then re-run the script.\n")
writeLines(c(header, as.yaml(out)), "data/publications.yml")
message(sprintf("Wrote %d publications (%d without topic tags).", length(out), untagged))
