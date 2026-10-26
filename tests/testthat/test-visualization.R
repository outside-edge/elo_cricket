testthat::test_that("chart endpoints equal published ratings and window starts carry past ratings", {
  matches <- subset(fixture_matches(), format == "Test")
  result <- calculate_elo(matches, as.Date("2025-12-31"))
  teams <- chart_teams(result$latest, "Test")
  start <- as.Date("2025-06-30")
  history <- chart_data(result$latest, result$updates, "Test", start)
  ends <- history[!duplicated(history$team_id, fromLast = TRUE), ]
  testthat::expect_equal(ends$rating, teams$rating[match(ends$team_id, teams$team_id)])
  testthat::expect_true(all(ends$date == as.Date("2025-12-31")))
  testthat::expect_equal(anyDuplicated(history[c("team_id", "date")]), 0L)
  testthat::expect_true(all(history$date >= start))
  baseline <- history[history$date == start, ]
  expected <- elo_snapshot(result$models$Test, start)
  testthat::expect_equal(baseline$rating, expected$rating[match(baseline$team_id, expected$team_id)])
})

testthat::test_that("plots preserve same-day update order and do not backdate new teams", {
  matches <- subset(fixture_matches(), format == "Test")[1:3, ]
  matches$match_id <- 1:3
  matches$start_date <- matches$end_date <- as.Date(c("2025-01-01", "2025-01-01", "2025-03-01"))
  matches$team1_id <- c(1L, 2L, 3L)
  matches$team2_id <- c(2L, 1L, 1L)
  matches$team1 <- c("A", "B", "New team")
  matches$team2 <- c("B", "A", "A")
  matches$score <- 1
  result <- calculate_elo(matches, as.Date("2025-04-01"))
  history <- chart_data(result$latest, result$updates, "Test")
  first <- history[history$date == as.Date("2025-01-01"), ]
  testthat::expect_equal(first$rating[first$team_id == 1L], 986)
  testthat::expect_equal(first$rating[first$team_id == 2L], 1014)
  cropped <- chart_data(result$latest, result$updates, "Test", as.Date("2025-02-01"))
  testthat::expect_equal(min(cropped$date[cropped$team_id == 3L]), as.Date("2025-03-01"))
  testthat::expect_equal(anyDuplicated(cropped[c("team_id", "date")]), 0L)
  early <- chart_data(result$latest, result$updates, "Test", as.Date("2024-01-01"))
  testthat::expect_equal(min(early$date), as.Date("2025-01-01"))
})
