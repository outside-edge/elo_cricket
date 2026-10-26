root <- normalizePath("../..")
source(file.path(root, "R/acquisition.R"))
source(file.path(root, "R/elo.R"))
source(file.path(root, "R/pipeline.R"))
source(file.path(root, "scripts/prepare_matches.R"))
fixture <- function(name) file.path(root, "tests/fixtures", name)
fixture_matches <- function() {
  rbind(
    parse_page(fixture("test_2025.html"), "Test", 2025),
    parse_page(fixture("t20i_2025.html"), "T20I", 2025)
  )
}
fixture_coverage <- formats[c(1, 3), ]
fixture_coverage$first_year <- 2025L
cache_fixture <- function(directory, format = "Test", year = 2025L) {
  file <- fixture(paste0(tolower(format), "_", year, ".html"))
  content <- readBin(file, "raw", n = file.info(file)$size)
  records <- parse_page(content, format, year)
  path <- file.path(directory, tolower(format), paste0(year, ".html.gz"))
  receipt <- list(
    url = source_url(format, year),
    request_url = sub("stats.espncricinfo.com", "stats.cricinfo.com", source_url(format, year), fixed = TRUE),
    format = format, year = year,
    fetched_at = "2026-01-01T00:00:00Z", rows = nrow(records),
    sha256 = digest::digest(content, algo = "sha256", serialize = FALSE)
  )
  write_page(path, content, receipt)
  path
}
