formats <- data.frame(
  format = c("Test", "ODI", "T20I"), class = 1:3,
  first_year = c(1877L, 1971L, 2005L),
  label = c("Test matches", "One-Day Internationals", "Twenty20 Internationals")
)

source_url <- function(format, year) {
  class <- formats$class[match(format, formats$format)]
  if (is.na(class)) stop("Unknown format: ", format)
  paste0(
    "https://stats.espncricinfo.com/ci/engine/records/team/",
    "match_results.html?class=", class, ";id=", year, ";type=year"
  )
}

parse_dates <- function(text) {
  text <- gsub("\\s*-\\s*", "-", trimws(gsub("\\s+", " ", text)))
  pattern <- paste0(
    "^([A-Z][a-z]{2}) ([0-9]{1,2})(?:, ([0-9]{4}))?",
    "(?:-(?:([A-Z][a-z]{2}) )?([0-9]{1,2}))?, ([0-9]{4})$"
  )
  fields <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(fields) != 7L) stop("Unrecognized match date: ", text)
  month <- match(fields[2], month.abb)
  end_month <- if (nzchar(fields[5])) match(fields[5], month.abb) else month
  end_year <- as.integer(fields[7])
  start_year <- if (nzchar(fields[4])) as.integer(fields[4]) else end_year - (month > end_month)
  end_day <- if (nzchar(fields[6])) as.integer(fields[6]) else as.integer(fields[3])
  dates <- as.Date(c(
    sprintf("%04d-%02d-%02d", start_year, month, as.integer(fields[3])),
    sprintf("%04d-%02d-%02d", end_year, end_month, end_day)
  ))
  if (anyNA(dates) || diff(dates) < 0 || diff(dates) > 20) stop("Invalid match date interval: ", text)
  dates
}

empty_matches <- function() {
  data.frame(
    match_id = integer(), format = character(),
    start_date = as.Date(character()), end_date = as.Date(character()),
    team1_id = integer(), team1 = character(), team2_id = integer(),
    team2 = character(), result = character(), score = numeric(), source_url = character()
  )
}

parse_page <- function(html, format, year) {
  page <- xml2::read_html(html)
  tables <- rvest::html_elements(page, "table.engineTable")
  captions <- rvest::html_text2(rvest::html_element(tables, "caption"))
  table <- tables[!is.na(captions) & captions == "Match results"]
  if (length(table) != 1L) stop("Expected one results table for ", format, " ", year)
  rows <- rvest::html_elements(table, "tr.data1")
  table_text <- trimws(gsub("\\s+", " ", rvest::html_text2(table)))
  explicit_empty <- !length(rows) && table_text == "Match results No records found"
  title <- rvest::html_text2(rvest::html_element(page, "title"))
  valid_title <- !is.na(title) && grepl(as.character(year), title, fixed = TRUE)
  generic_empty <- explicit_empty && identical(title, "Cricket Team Records & Stats | Cricinfo.com")
  if (!valid_title && !generic_empty) {
    stop("Wrong source year: ", title)
  }
  description <- rvest::html_attr(rvest::html_element(page, "meta[name='description']"), "content")
  label <- formats$label[match(format, formats$format)]
  if (is.na(description) || !grepl(tolower(label), tolower(description), fixed = TRUE)) {
    stop("Wrong source format: expected ", format)
  }
  if (!length(rows)) {
    if (explicit_empty) {
      return(empty_matches())
    }
    stop("Empty table without an explicit no-records message")
  }
  headers <- rvest::html_text2(rvest::html_elements(table, "thead th"))
  if (!identical(headers, c("Team 1", "Team 2", "Winner", "Margin", "Ground", "Match Date", "Scorecard"))) {
    stop("Results columns changed")
  }
  links <- tolower(rvest::html_text2(rvest::html_elements(page, "a[href]")))
  if (any(links %in% c("next", "next >", "next >>"))) stop("Source became paginated")
  identity <- function(cell, kind) {
    link <- rvest::html_elements(cell, paste0("a[href*='/", kind, "/']"))
    if (length(link) != 1L) stop("Missing or ambiguous ", kind, " identity")
    href <- rvest::html_attr(link, "href")
    pattern <- paste0(".*/", kind, "/([0-9]+)\\.html$")
    if (!grepl(pattern, href)) stop("Invalid ", kind, " identity")
    list(id = as.integer(sub(pattern, "\\1", href)), name = rvest::html_text2(link))
  }
  records <- lapply(rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    n <- length(cells)
    if (!n %in% c(6L, 7L)) stop("Wrong number of result cells")
    a <- identity(cells[1], "team")
    b <- identity(cells[2], "team")
    if (a$id == b$id || !nzchar(a$name) || !nzchar(b$name)) stop("Invalid pair of teams")
    result <- tolower(rvest::html_text2(cells[3]))
    if (length(rvest::html_elements(cells[3], "a[href*='/team/']"))) {
      winner <- identity(cells[3], "team")$id
      if (!winner %in% c(a$id, b$id)) stop("Winner is not a participant")
      score <- as.numeric(winner == a$id)
      result <- "win"
    } else if (result %in% c("drawn", "tied")) {
      score <- 0.5
    } else if (result %in% c("no result", "abandoned", "cancelled")) {
      score <- NA_real_
    } else {
      stop("Unrecognized result: ", result)
    }
    dates <- parse_dates(rvest::html_text2(cells[n - 1L]))
    if (!year %in% seq.int(as.integer(format(dates[1], "%Y")), as.integer(format(dates[2], "%Y")))) {
      stop("Match outside source year")
    }
    id <- identity(cells[n], "match")$id
    data.frame(
      match_id = id, format = format, start_date = dates[1], end_date = dates[2],
      team1_id = a$id, team1 = a$name, team2_id = b$id, team2 = b$name,
      result = result, score = score,
      source_url = paste0("https://www.espncricinfo.com/ci/engine/match/", id, ".html")
    )
  })
  records <- do.call(rbind, records)
  if (anyDuplicated(records$match_id)) stop("Duplicate match IDs within source page")
  records
}

receipt_time <- function(receipt) {
  required <- c("url", "request_url", "format", "year", "fetched_at", "sha256", "rows")
  if (!all(required %in% names(receipt)) || any(vapply(receipt[required], function(x) {
    length(x) != 1L || anyNA(x)
  }, logical(1)))) stop("Incomplete source receipt")
  stamp <- receipt$fetched_at
  if (!is.character(stamp) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", stamp)) {
    stop("Invalid source retrieval timestamp")
  }
  fetched <- as.POSIXct(stamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(fetched) || format(fetched, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != stamp) {
    stop("Invalid source retrieval timestamp")
  }
  fetched
}

validate_source_freshness <- function(receipt, as_of, now = Sys.time()) {
  fetched <- receipt_time(receipt)
  if (receipt$year < as.integer(format(as_of, "%Y")) - 1L) return(invisible(NULL))
  cutoff_end <- as.POSIXct(as.character(as_of + 1L), tz = "UTC")
  if (fetched < cutoff_end) stop("Source retrieval predates the completed cutoff day")
  age <- as.numeric(difftime(now, fetched, units = "days"))
  if (age < 0 || age > 8) stop("Stale source")
  invisible(NULL)
}

read_page <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  content <- readBin(con, "raw", n = 100000000L)
  receipt <- jsonlite::read_json(sub("\\.gz$", ".json", path), simplifyVector = TRUE)
  receipt_time(receipt)
  if (!identical(digest::digest(content, algo = "sha256", serialize = FALSE), receipt$sha256)) {
    stop("Source checksum mismatch: ", path)
  }
  list(content = content, receipt = receipt)
}

write_page <- function(path, content, receipt) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  con <- gzfile(temporary, "wb")
  tryCatch(writeBin(content, con), finally = close(con))
  receipt_path <- sub("\\.gz$", ".json", path)
  jsonlite::write_json(receipt, paste0(receipt_path, ".tmp"), auto_unbox = TRUE, pretty = TRUE)
  if (!file.rename(temporary, path)) stop("Cannot replace raw source")
  if (!file.rename(paste0(receipt_path, ".tmp"), receipt_path)) stop("Cannot replace source receipt")
}

request_page <- function(url) {
  request <- httr2::request(url) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_timeout(90) |>
    httr2::req_throttle(capacity = 1, fill_time_s = 6) |>
    httr2::req_retry(
      max_tries = 8, retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    )
  httr2::resp_body_raw(httr2::req_perform(request))
}

fetch_sources <- function(raw_dir, as_of, refresh = FALSE, coverage = formats, request = request_page) {
  last_year <- as.integer(format(as_of, "%Y"))
  for (i in seq_len(nrow(coverage))) {
    format <- coverage$format[i]
    for (year in seq.int(coverage$first_year[i], last_year)) {
      path <- file.path(raw_dir, tolower(format), paste0(year, ".html.gz"))
      previous <- tryCatch(read_page(path), error = function(e) NULL, warning = function(w) NULL)
      if (!is.null(previous) && !refresh && year < last_year - 1L) next
      url <- source_url(format, year)
      request_url <- sub("stats.espncricinfo.com", "stats.cricinfo.com", url, fixed = TRUE)
      content <- request(request_url)
      records <- parse_page(content, format, year)
      if (!is.null(previous)) {
        before <- parse_page(previous$content, format, year)
        if (length(setdiff(before$match_id, records$match_id))) {
          stop("Previously recorded matches disappeared from ", format, " ", year, "; review source changes")
        }
      }
      receipt <- list(
        url = url, request_url = request_url, format = format, year = year,
        fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        sha256 = digest::digest(content, algo = "sha256", serialize = FALSE), rows = nrow(records)
      )
      write_page(path, content, receipt)
      message("Fetched ", format, " ", year, ": ", nrow(records), " results")
    }
  }
}

load_matches <- function(raw_dir, as_of, require_fresh = FALSE, coverage = formats) {
  records <- list()
  receipts <- list()
  last_year <- as.integer(format(as_of, "%Y"))
  for (i in seq_len(nrow(coverage))) {
    format <- coverage$format[i]
    for (year in seq.int(coverage$first_year[i], last_year)) {
      path <- file.path(raw_dir, tolower(format), paste0(year, ".html.gz"))
      cached <- read_page(path)
      receipt <- cached$receipt
      if (
        !identical(receipt$url, source_url(format, year)) ||
          !identical(receipt$format, format) || receipt$year != year
      ) {
        stop("Wrong source metadata: ", path)
      }
      if (require_fresh) validate_source_freshness(receipt, as_of)
      parsed <- parse_page(cached$content, format, year)
      if (nrow(parsed) != receipt$rows) stop("Source row count changed: ", path)
      records[[length(records) + 1L]] <- parsed[parsed$end_date <= as_of, ]
      receipts[[length(receipts) + 1L]] <- receipt
    }
  }
  matches <- unique(do.call(rbind, records))
  if (anyDuplicated(matches$match_id)) stop("Conflicting duplicate match")
  matches <- matches[order(matches$end_date, matches$start_date, matches$match_id), ]
  rownames(matches) <- NULL
  list(matches = matches, sources = receipts, as_of = as_of)
}
