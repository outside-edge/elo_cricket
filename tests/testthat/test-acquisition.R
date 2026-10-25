testthat::test_that("date ranges preserve actual completion dates", {
  cases <- list(
    c("Jan 2-6, 2025", "2025-01-02", "2025-01-06"),
    c("Jan 29-Feb 1, 2025", "2025-01-29", "2025-02-01"),
    c("Dec 31, 1881 - Jan 4, 1882", "1881-12-31", "1882-01-04"),
    c("Jan 5, 1971", "1971-01-05", "1971-01-05")
  )
  for (case in cases) testthat::expect_equal(parse_dates(case[1]), as.Date(case[-1]))
  testthat::expect_error(parse_dates("unknown"), "Unrecognized")
  testthat::expect_error(parse_dates("Jan 29-1, 2025"), "Invalid")
})

testthat::test_that("complete source tables retain draws and no-results", {
  matches <- fixture_matches()
  testthat::expect_equal(sum(matches$format == "Test"), 42)
  testthat::expect_equal(sum(matches$format == "T20I"), 579)
  testthat::expect_equal(matches$score[matches$match_id == 1485503], 0.5)
  testthat::expect_equal(matches$end_date[matches$match_id == 1485503], as.Date("2025-06-21"))
  testthat::expect_true(all(is.na(matches$score[matches$result == "no result"])))
  testthat::expect_equal(sum(!is.na(matches$score)), 597)
})

testthat::test_that("only explicit no-records pages count as empty years", {
  testthat::expect_equal(nrow(parse_page(fixture("test_1878.html"), "Test", 1878)), 0)
  html <- paste(readLines(fixture("test_1878.html"), warn = FALSE), collapse = "\n")
  wartime <- sub("1878 Cricket Team Records", "Cricket Team Records", html, fixed = TRUE)
  testthat::expect_equal(nrow(parse_page(wartime, "Test", 1916)), 0)
  testthat::expect_error(parse_page(sub("No records found", "", html), "Test", 1878), "explicit no-records")
  testthat::expect_error(parse_page("<html>Access denied</html>", "Test", 1878), "Expected one")
})

testthat::test_that("unknown outcomes, wrong pools, and truncated tables fail", {
  html <- paste(readLines(fixture("test_2025.html"), warn = FALSE), collapse = "\n")
  testthat::expect_error(parse_page(sub(">drawn<", ">unknown<", html), "Test", 2025), "Unrecognized result")
  testthat::expect_error(parse_page(html, "ODI", 2025), "Wrong source format")
  testthat::expect_error(parse_page(html, "Test", 2024), "Wrong source year")
  testthat::expect_error(parse_page(sub("</body>", "<a href='next'>Next</a></body>", html), "Test", 2025), "paginated")
  page <- xml2::read_html(html)
  row <- rvest::html_element(page, "tr.data1")
  xml2::xml_add_sibling(row, row)
  testthat::expect_error(parse_page(as.character(page), "Test", 2025), "Duplicate match IDs")
})

testthat::test_that("cache checksums, coverage, and cutoff are enforced", {
  directory <- withr::local_tempdir()
  path <- cache_fixture(directory)
  coverage <- fixture_coverage[1, ]
  testthat::expect_equal(nrow(load_matches(directory, as.Date("2025-12-31"), coverage = coverage)$matches), 42)
  testthat::expect_equal(nrow(load_matches(directory, as.Date("2025-01-04"), coverage = coverage)$matches), 0)
  testthat::expect_error(load_matches(directory, as.Date("2025-12-31"), TRUE, coverage), "Stale source")
  testthat::expect_error(suppressWarnings(load_matches(directory, as.Date("2026-12-31"), coverage = coverage)))
  receipt_path <- sub("\\.gz$", ".json", path)
  receipt <- jsonlite::read_json(receipt_path)
  receipt$sha256 <- "changed"
  write_json(receipt, receipt_path)
  testthat::expect_error(read_page(path), "checksum")
})

testthat::test_that("fetch failures and removed rows preserve the existing cache", {
  directory <- withr::local_tempdir()
  path <- cache_fixture(directory)
  before <- sha256(path)
  coverage <- fixture_coverage[1, ]
  testthat::expect_error(
    fetch_sources(directory, as.Date("2025-12-31"), coverage = coverage,
                  request = function(url) stop("unavailable")),
    "unavailable"
  )
  testthat::expect_identical(sha256(path), before)
  testthat::expect_error(
    fetch_sources(directory, as.Date("2025-12-31"), coverage = coverage,
                  request = function(url) charToRaw("<html>Denied</html>")),
    "Expected one"
  )
  testthat::expect_identical(sha256(path), before)
  page <- xml2::read_html(fixture("test_2025.html"))
  xml2::xml_remove(rvest::html_element(page, "tr.data1"))
  testthat::expect_error(
    fetch_sources(directory, as.Date("2025-12-31"), coverage = coverage,
                  request = function(url) charToRaw(as.character(page))),
    "disappeared"
  )
  testthat::expect_identical(sha256(path), before)
})

testthat::test_that("legacy BOM and 415 incorrect outcomes are repaired", {
  matches <- prepare_matches(file.path(root, "data/cricket_matches_for_elo.csv"))
  testthat::expect_equal(nrow(matches), 7735)
  testthat::expect_equal(sum(grepl("tied", matches$outcome)), 60)
  testthat::expect_true(all(matches$score[matches$drawn] == 0.5))
  testthat::expect_false(any(grepl("no result|abandoned|cancelled", tolower(matches$outcome))))
  testthat::expect_equal(length(matches$team1), nrow(matches))
})
