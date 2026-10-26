test_that("context joins validate identities and distinguish neutral from unknown", {
  matches <- simulated_matches(3)
  matches$team1 <- "England"
  matches$team2 <- "India"
  profiles <- data.frame(match_id = 1:3, ground_profile_id = c(10, 10, 20), ground_label = c("A", "A", "B"))
  legacy <- data.frame(match_id = 1L, team1 = "India", team2 = "England", ground = "Full ground A")
  grounds <- data.frame(ground = "Full ground A", country = "United Arab Emirates")
  result <- join_match_context(matches, profiles, legacy, grounds)
  expect_equal(result$matches$home_sign, c(0L, 0L, NA_integer_))
  expect_equal(result$matches$country[1:2], rep("United Arab Emirates", 2))
  legacy$team1 <- "Australia"
  expect_error(join_match_context(matches, profiles, legacy, grounds), "participants disagree")
  legacy$team1 <- "India"
  expect_error(join_match_context(matches, rbind(profiles, profiles[1, ]), legacy, grounds), "unique match")
  grounds <- rbind(grounds, transform(grounds, country = "India"))
  expect_error(join_match_context(matches, profiles, legacy, grounds), "Conflicting countries")
  grounds <- data.frame(ground = "Full ground A", country = " U.A.E. ")
  matches$team1 <- "U.A.E."
  legacy$team2 <- "U.A.E."
  expect_equal(join_match_context(matches, profiles, legacy, grounds)$matches$home_sign[1:2], c(1L, 1L))
})

test_that("forecast splits exclude ongoing games and no-results", {
  matches <- simulated_matches()
  origin <- as.Date("2008-01-01")
  matches$start_date[1:3] <- origin + c(-4, -3, 2)
  matches$end_date[1:3] <- origin + c(1, -1, 6)
  matches$score[3] <- NA_real_
  result <- forecast_split(matches, origin, as.Date("2010-01-01"))
  expect_false(1L %in% result$train$match_id)
  expect_true(2L %in% result$train$match_id)
  expect_false(3L %in% result$test$match_id)
  expect_true(all(result$train$end_date < origin))
  changed <- matches
  changed$score[changed$start_date >= origin] <- 1 - changed$score[changed$start_date >= origin]
  expect_equal(forecast_split(changed, origin, as.Date("2010-01-01"))$train, result$train)
})

test_that("scoring uses the full categorical outcome", {
  p <- rbind(c(0.2, 0.3, 0.5), c(0.7, 0.2, 0.1))
  result <- probability_metrics(p, c(0.5, 0))
  expect_equal(result$log_loss, -mean(log(c(0.3, 0.7))))
  expect_equal(result$brier, mean(c(0.2^2 + 0.7^2 + 0.5^2, 0.3^2 + 0.2^2 + 0.1^2)))
  expect_error(probability_metrics(p, c(NA, 0)), "Missing")
  expect_error(check_probabilities(p + 0.1), "Invalid")
  expect_equal(probability_metrics(matrix(c(0.5, 0, 0.5), 1), 1)$accuracy, 0.5)
})

test_that("failed model runs preserve the previous result and diagnostic files", {
  directory <- withr::local_tempdir()
  output <- file.path(directory, "result")
  dir.create(output)
  writeLines("previous", file.path(output, "result.txt"))
  expect_error(
    suppressMessages(replace_model_directory(output, function(staging) {
      writeLines("diagnostic evidence", file.path(staging, "draws.csv"))
      stop("sampling failed")
    })), "sampling failed"
  )
  expect_equal(readLines(file.path(output, "result.txt")), "previous")
  failed <- list.files(directory, pattern = "^failed-", full.names = TRUE)
  expect_length(failed, 1L)
  expect_equal(readLines(file.path(failed, "draws.csv")), "diagnostic evidence")
})

test_that("baseline calibration histories respect the forecast horizon", {
  train <- simulated_matches(200L)
  origin <- as.Date("2010-01-01")
  train <- train[train$end_date < origin, ]
  change <- format(train$start_date, "%Y") == "2008"
  altered <- train
  altered$score[change] <- 1 - altered$score[change]
  elo <- fit_elo_history(train, origin)
  expect_equal(elo$predictor[change], fit_elo_history(altered, origin)$predictor[change])
  glicko <- fit_glicko_history(train, origin)
  expect_equal(glicko$predictor[change], fit_glicko_history(altered, origin)$predictor[change])
  p <- predict_home_climatology(train, train)
  expect_equal(unname(p), unname(predict_home_climatology(train, swap_teams(train))[, 3:1]))
  x <- elo$predictor
  calibration <- fit_score_calibration(x, train, origin, with_home = TRUE)
  p <- predict_score_calibration(calibration, x, train$home_sign)
  reversed <- predict_score_calibration(calibration, -x, -train$home_sign)
  expect_equal(unname(p), unname(reversed[, 3:1]), tolerance = 1e-10)
})

test_that("paired splines recover strength ordering and reverse under swapping", {
  matches <- simulated_matches()
  fit <- fit_spline(matches, margin = 0.5, k = 4L)
  p <- predict_spline(fit, matches)
  reversed <- predict_spline(fit, swap_teams(matches))
  expect_equal(unname(p), unname(reversed[, 3:1]), tolerance = 1e-8)
  expect_true(all(abs(rowSums(p) - 1) < 1e-10))
  expect_gt(mean(p[matches$team1_id == 4 & matches$team2_id == 1, 3]), 0.75)
  expect_lt(mean(p[matches$team1_id == 1 & matches$team2_id == 4, 3]), 0.25)
  future <- matches[1:3, ]
  future$start_date <- max(matches$start_date) + 30L
  later <- future
  later$start_date <- future$start_date + 365L
  expect_equal(predict_spline(fit, future), predict_spline(fit, later))
  future$country <- "Unseen country"
  future$ground_profile_id <- 999L
  expect_silent(check_probabilities(predict_spline(fit, future)))
  future$team1_id <- 999L
  expect_error(predict_spline(fit, future), "teams represented")
  matches$country <- "England"
  matches$ground_profile_id <- 1L
  single <- fit_spline(matches, margin = 0.5, k = 4L)
  expect_silent(check_probabilities(predict_spline(single, matches)))
})

test_that("Glicko predictions handle empty periods, new sides, and swapping", {
  matches <- simulated_matches(100L)
  games <- data.frame(period = 1, a = matches$team1_id, b = matches$team2_id, score = matches$score)
  status <- PlayerRatings::glicko2(games, gamma = 0)$ratings
  before <- glicko_predictor(status, matches)
  after <- glicko_predictor(glicko_empty_month(status), matches)
  expect_true(all(abs(after) <= abs(before) + 1e-10))
  expect_equal(before, -glicko_predictor(status, swap_teams(matches)), tolerance = 1e-10)
  expect_equal(after, glicko_predictor(status, matches, extra_months = 1L))
  matches$team1_id <- 999L
  expect_true(all(is.finite(glicko_predictor(status, matches))))
  expect_error(fit_glicko_history(matches, as.Date("2010-01-02")), "boundaries")
})

test_that("Elo forecasts use completed matches and are frozen at origin", {
  train <- simulated_matches(100L)
  model <- fit_elo(train)
  test <- train[1:4, ]
  before <- elo_predictor(model, test, min(train$start_date))
  expect_equal(before, rep(0, 4))
  origin <- max(train$end_date) + 1
  p <- elo_predictor(model, test, origin)
  expect_equal(p, -elo_predictor(model, swap_teams(test), origin))
  test$start_date <- test$start_date + 365L
  expect_equal(p, elo_predictor(model, test, origin))
})

test_that("Stan data contain no holdout outcomes and preserve calendar gaps", {
  matches <- simulated_matches(100L)
  train <- matches[1:80, ]
  test <- matches[81:100, ]
  original <- stan_inputs(train, test)
  test$score <- 1 - test$score
  expect_identical(stan_inputs(train, test), original)
  expect_length(original$data$y, 80)
  expect_equal(original$data$Q, max(quarter_index(matches$start_date)) - min(quarter_index(matches$start_date)) + 1L)
  reversed <- stan_inputs(swap_teams(train), swap_teams(test))
  expect_equal(reversed$data$a, original$data$b)
  expect_equal(reversed$data$home, -original$data$home)
  expect_equal(reversed$data$country, original$data$country)
})
