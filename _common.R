# _common.R: shared helpers sourced by every page.
# Reads data/*.yml and renders HTML snippets, so pages stay layout-only
# and every number on the site comes from the data files.
library(yaml)

read_data <- function(name) {
  x <- yaml::read_yaml(file.path("data", paste0(name, ".yml")))
  if (is.null(x)) list() else x
}

`%||%` <- function(a, b) if (is.null(a) || identical(a, "")) b else a

# Initials for people without a headshot yet
initials <- function(name) {
  parts <- strsplit(gsub(",.*$", "", name), "\\s+")[[1]]
  paste(substr(parts[c(1, length(parts))], 1, 1), collapse = "")
}

person_card <- function(p) {
  pic <- if (nzchar(p$photo %||% "")) {
    sprintf('<img src="images/people/%s" alt="%s">', p$photo, p$name)
  } else sprintf('<span class="ph">%s</span>', initials(p$name))
  links <- person_links(p)
  bio <- if (nzchar(p$bio %||% "")) sprintf('<p class="bio">%s</p>', p$bio) else '<p class="bio tbd">Bio coming soon.</p>'
  sprintf('<div class="person">%s<h3>%s</h3><div class="role">%s</div><div class="aff">%s</div>%s%s</div>',
          pic, p$name, p$role, p$affiliation %||% "", links, bio)
}

# Google Scholar mark (Simple Icons path), inline so no extra font is needed
scholar_svg <- '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M5.242 13.769L0 9.5 12 0l12 9.5-5.242 4.269C17.548 11.249 14.978 9.5 12 9.5c-2.977 0-5.548 1.748-6.758 4.269zM12 10a7 7 0 1 0 0 14 7 7 0 0 0 0-14z"/></svg>'

# Icon row under each person: email, Google Scholar, and faculty profile (globe).
# An icon only appears when its URL is filled in people.yml.
person_links <- function(p) {
  out <- character(0)
  if (nzchar(p$email %||% ""))
    out <- c(out, sprintf('<a href="mailto:%s" title="%s" aria-label="Email %s"><i class="bi bi-envelope"></i></a>', p$email, p$email, p$name))
  if (nzchar(p$scholar %||% ""))
    out <- c(out, sprintf('<a href="%s" title="Google Scholar" aria-label="%s on Google Scholar">%s</a>', p$scholar, p$name, scholar_svg))
  if (nzchar(p$profile %||% ""))
    out <- c(out, sprintf('<a href="%s" title="Faculty profile" aria-label="%s faculty profile"><i class="bi bi-globe2"></i></a>', p$profile, p$name))
  if (!length(out)) return("")
  sprintf('<div class="links">%s</div>', paste(out, collapse = ""))
}

people_grid <- function(group) {
  ppl <- Filter(function(p) identical(p$group, group), read_data("people"))
  if (!length(ppl)) return(cat('<p class="tbd">Coming soon.</p>'))
  cat('<div class="people-grid">', vapply(ppl, person_card, ""), '</div>', sep = "\n")
}

fmt_dollars <- function(x) paste0("$", formatC(x, format = "d", big.mark = ","))

# Site-wide counts, all derived from the data files
site_stats <- function() {
  ppl <- read_data("people"); gr <- read_data("grants"); pr <- read_data("projects")
  list(
    people   = length(ppl),
    fellows  = sum(vapply(ppl, function(p) identical(p$group, "fellow"), TRUE)),
    projects = length(pr),
    grants_n = length(gr),
    grants_d = sum(vapply(gr, function(g) as.numeric(g$amount %||% 0), 0))
  )
}

# ---- Recent updates (home page) ----
# Reads data/news.yml or data/publications.yml, sorts newest first, shows the top n.
# These files are written by hand for now; a script can later regenerate them
# automatically (e.g., from Google Scholar/ORCID for research, an RSS feed for news)
# without any change to the page.
update_list <- function(name, n = 4, empty = "Coming soon.") {
  items <- read_data(name)
  if (!length(items)) return(cat(sprintf('<p class="tbd">%s</p>', empty)))
  items <- items[order(vapply(items, item_sort_key, ""), decreasing = TRUE)][seq_len(min(n, length(items)))]
  cat('<ul class="update-list">')
  for (i in items) {
    title <- if (nzchar(i$url %||% "")) sprintf('<a href="%s">%s</a>', i$url, esc(i$title)) else esc(i$title)
    # Month + year when a full date exists, otherwise just the year
    when  <- if (nzchar(as.character(i$date %||% ""))) format(as.Date(as.character(i$date)), "%B %Y") else as.character(i$year)
    extra <- if (nzchar(i$source %||% "")) sprintf(' · <em>%s</em>', esc(i$source)) else ""
    who   <- if (nzchar(i$authors %||% "")) sprintf('<div class="who">%s</div>', esc(i$authors)) else ""
    cat(sprintf('<li><div class="when">%s%s</div><div class="what">%s</div>%s</li>', when, extra, title, who))
  }
  cat('</ul>')
}

# Sort key: full date if given, else year (treated as Jan 1 of that year)
item_sort_key <- function(i) {
  d <- as.character(i$date %||% "")
  if (nzchar(d)) d else paste0(i$year %||% "0000", "-01-01")
}

# Minimal HTML escaping for text pulled from YAML
esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x %||% ""), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

# Name without the degree ("Seth Watts, Ph.D." -> "Seth Watts"); used as the people tag
short_name <- function(name) trimws(sub(",.*$", "", name))
slug <- function(x) gsub("[^a-z0-9]+", "-", tolower(x))

# ---- Research page ----
# Writes every publication as HTML, grouped by year (newest first). Each item
# carries data-* attributes that the page's script uses for search, tag
# filtering, and the per-year "Show more" button.
pub_browser <- function(per_year = 5) {
  pubs <- read_data("publications")
  if (!length(pubs)) return(cat('<p class="tbd">Publications from our fellows are on the way.</p>'))

  center <- vapply(Filter(function(p) p$group %in% c("leadership", "fellow"), read_data("people")),
                   function(p) short_name(p$name), "")
  # Flag people tags that don't match anyone in people.yml (typos)
  bad <- setdiff(unique(unlist(lapply(pubs, `[[`, "people"))), center)
  if (length(bad)) warning("publications.yml: unknown people tag(s): ", paste(bad, collapse = ", "))

  topics <- sort(unique(unlist(lapply(pubs, `[[`, "topics"))))
  # Types in a fixed, sensible order; only types that actually occur are shown
  type_order <- c("Journal article", "Book", "Book chapter", "Brief", "Preprint", "Report")
  types  <- type_order[type_order %in% vapply(pubs, function(p) p$type %||% "", "")]
  people <- center[center %in% unlist(lapply(pubs, `[[`, "people"))]
  chips <- function(vals, kind) paste(sprintf(
    '<button type="button" class="chip" data-kind="%s" data-val="%s">%s</button>', kind, slug(vals), esc(vals)), collapse = "")

  pubs  <- pubs[order(vapply(pubs, item_sort_key, ""), decreasing = TRUE)]
  years <- unique(vapply(pubs, function(p) as.character(p$year), ""))

  # Search bar sits under the page title, above the list and filters
  cat('<div class="pub-layout"><div class="pub-main">',
      '<input type="search" id="pub-search" placeholder="Search publications by title, author, journal, or topic" aria-label="Search publications">', sep = "")
  for (y in years) {
    cat(sprintf('<section class="pub-year" data-year="%s"><h2>%s</h2><ol class="pub-list">', y, y))
    for (p in Filter(function(p) as.character(p$year) == y, pubs)) {
      title <- if (nzchar(p$url %||% "")) sprintf('<a href="%s">%s</a>', p$url, esc(p$title)) else esc(p$title)
      tags  <- paste(sprintf('<span class="tag">%s</span>', esc(unlist(p$topics))), collapse = "")
      search_text <- tolower(paste(p$title, p$authors, p$source %||% "", p$type %||% "", paste(unlist(p$topics), collapse = " "), paste(unlist(p$people), collapse = " ")))
      src_line <- paste(c(if (nzchar(p$source %||% "")) sprintf("<em>%s</em>", esc(p$source)),
                         if (nzchar(p$type %||% "")) sprintf('<span class="pub-type">%s</span>', esc(p$type))), collapse = " · ")
      cat(sprintf('<li class="pub" data-topics="%s" data-people="%s" data-type="%s" data-text="%s"><div class="pub-title">%s</div><div class="pub-authors">%s</div><div class="pub-source">%s</div><div class="pub-tags">%s</div></li>',
        paste(slug(unlist(p$topics)), collapse = " "), paste(slug(unlist(p$people)), collapse = " "), slug(p$type %||% ""), esc(search_text),
        title, esc(p$authors), src_line, tags))
    }
    cat('</ol><button type="button" class="show-more" hidden></button></section>')
  }
  cat('<p class="no-results tbd" hidden>No publications match your search.</p></div>')

  # Right-hand sidebar: people and topic filters
  cat('<aside class="pub-side">',
      if (length(people)) sprintf('<h4>People</h4><div class="chips">%s</div>', chips(people, "people")) else "",
      if (length(types))  sprintf('<h4>Type</h4><div class="chips">%s</div>', chips(types, "type")) else "",
      if (length(topics)) sprintf('<h4>Topics</h4><div class="chips">%s</div>', chips(topics, "topics")) else "",
      '<button type="button" id="pub-clear" class="clear-btn" hidden>Clear filters</button>',
      '</aside></div>', sep = "")
  cat(sprintf('<script>window.PUBS_PER_YEAR = %d;</script>', per_year))
}
