testthat::test_that("site checks resolve project URLs and reject missing or escaping assets", {
  directory <- withr::local_tempdir()
  dir.create(file.path(directory, "docs"))
  writeLines('<a href="docs/ratings.html">Ratings</a>', file.path(directory, "index.html"))
  testthat::expect_error(check_site(directory), "Broken site link")
  page <- file.path(directory, "docs/ratings.html")
  writeLines('<a href="/elo_cricket/">Home</a><img src="../chart.svg">', page)
  testthat::expect_error(check_site(directory), "chart.svg")
  writeLines("<svg/>", file.path(directory, "chart.svg"))
  testthat::expect_equal(check_site(directory), 2)
  writeLines('<a href="/wrong/index.html">Wrong base</a>', page)
  testthat::expect_error(check_site(directory), "outside site base path")
  outside <- tempfile(tmpdir = dirname(directory), fileext = ".html")
  withr::defer(unlink(outside))
  writeLines("outside", outside)
  writeLines(sprintf('<a href="../../%s">Outside</a>', basename(outside)), page)
  testthat::expect_error(check_site(directory), "escapes site")
})

testthat::test_that("current publication verifies downloads and rejects mixed cutoffs", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "latest.csv")
  rows <- data.frame(
    model = c("gam", "stan_dynamic_probit", "glicko2", "trueskill"),
    team_id = 1L, as_of = as.Date("2026-10-25")
  )
  write_fixture <- function() {
    readr::write_csv(rows, path)
    required <- c("coverage.csv", "strengths.csv", "stan_diagnostics.json",
      as.vector(outer(c("gam", "stan_dynamic_probit", "glicko2", "trueskill"), c(".png", ".svg"), paste0))
    )
    for (name in required) writeLines("fixture", file.path(directory, name))
    files <- file.path(directory, c("latest.csv", required))
    jsonlite::write_json(list(
      stan_diagnostics_passed = TRUE, as_of = "2026-10-25",
      files = setNames(lapply(files, digest::digest, algo = "sha256", file = TRUE), basename(files))
    ), file.path(directory, "manifest.json"), auto_unbox = TRUE)
  }
  write_fixture()
  testthat::expect_equal(nrow(read_current_publication(directory)$latest), 4L)
  cat("tampering", file = path, append = TRUE)
  testthat::expect_error(read_current_publication(directory), "checksum mismatch")
  rows$as_of[1] <- as.Date("2024-01-01")
  write_fixture()
  testthat::expect_error(read_current_publication(directory), "cutoff")
  rows$as_of[1] <- as.Date("2026-10-25")
  rows <- rbind(rows, rows[1, ])
  write_fixture()
  testthat::expect_error(read_current_publication(directory), "Duplicate")
})
