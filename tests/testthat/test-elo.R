testthat::test_that("all 100 reference ratings agree exactly", {
  result <- calculate_elo(fixture_matches(), as.Date("2025-12-31"))
  reference <- read.csv(fixture("elo_reference.csv"))
  actual <- result$latest[c("format", "team_id", "rating")]
  actual <- actual[order(actual$format, actual$team_id), ]
  reference <- reference[order(reference$format, reference$team_id), ]
  rownames(actual) <- rownames(reference) <- NULL
  testthat::expect_equal(actual, reference)
  testthat::expect_equal(nrow(result$updates), 597)
  before <- result$updates$team1_before + result$updates$team2_before
  after <- result$updates$team1_after + result$updates$team2_after
  testthat::expect_equal(before, after)
})

testthat::test_that("future matches and future entrants cannot affect historical snapshots", {
  matches <- subset(fixture_matches(), format == "Test")
  full <- fit_elo(matches)
  for (date in as.list(as.Date(c("2025-01-31", "2025-03-31", "2025-06-30")))) {
    truncated <- fit_elo(matches[matches$end_date <= date, ])
    testthat::expect_equal(elo_snapshot(full, date), elo_snapshot(truncated, date))
  }
  synthetic <- matches[1:3, ]
  synthetic$start_date <- synthetic$end_date <- as.Date(c("2025-01-01", "2025-02-01", "2025-03-01"))
  synthetic$team1_id <- c(1L, 2L, 3L)
  synthetic$team2_id <- c(2L, 1L, 1L)
  synthetic$team1 <- c("A", "B", "Future")
  synthetic$team2 <- c("B", "A", "A")
  synthetic$score <- 1
  snapshot <- elo_snapshot(fit_elo(synthetic), as.Date("2025-01-15"))
  testthat::expect_setequal(snapshot$team_id, 1:2)
  testthat::expect_equal(snapshot$rating[snapshot$team_id == 1], 1050)
})

testthat::test_that("no-results have no effect and ties are symmetric", {
  matches <- subset(fixture_matches(), format == "T20I")
  testthat::expect_equal(fit_elo(matches)$updates, fit_elo(matches[!is.na(matches$score), ])$updates)
  tie <- matches[1, ]
  tie$score <- 0.5
  tie$result <- "tied"
  testthat::expect_equal(fit_elo(tie)$updates$team1_after, 1000)
  testthat::expect_equal(fit_elo(tie)$updates$team2_after, 1000)
})

testthat::test_that("stable IDs survive aliases and input order does not matter", {
  matches <- subset(fixture_matches(), format == "Test")
  original <- fit_elo(matches)
  matches$team1 <- paste0(matches$team1, " alias")
  testthat::expect_equal(fit_elo(matches[rev(seq_len(nrow(matches))), ])$updates, original$updates)
  testthat::expect_equal(nrow(elo_snapshot(original, as.Date("2028-12-31"))), 12)
  testthat::expect_true(all(is.na(elo_snapshot(original, as.Date("2028-12-31"))$rank)))
})

testthat::test_that("monthly snapshots are actual completed calendar month ends", {
  matches <- subset(fixture_matches(), format == "Test")
  result <- calculate_elo(matches, as.Date("2025-10-24"))
  testthat::expect_true(all(format(result$monthly$date + 1L, "%d") == "01"))
  testthat::expect_true(max(result$monthly$date) < as.Date("2025-10-01"))
  testthat::expect_true(all(result$latest$last_rated_match <= as.Date("2025-10-24")))
})

testthat::test_that("same-day snapshots retain fitting order and partial months stay empty", {
  matches <- subset(fixture_matches(), format == "Test")[1:2, ]
  matches$match_id <- c(2L, 1L)
  matches$start_date <- as.Date(c("2025-01-01", "2025-01-02"))
  matches$end_date <- as.Date("2025-01-05")
  matches$team1_id <- 1L
  matches$team2_id <- 2L
  matches$score <- c(1, 0)
  model <- fit_elo(matches)
  snapshot <- elo_snapshot(model, as.Date("2025-01-05"))
  testthat::expect_equal(snapshot$rating[snapshot$team_id == 1], 986)
  result <- calculate_elo(matches, as.Date("2025-01-05"))
  testthat::expect_equal(nrow(result$monthly), 0)
  testthat::expect_equal(names(result$monthly), names(result$latest))
})
