run_model_comparison <- function(format, origin, until, iterations = 1000L) {
  origin <- as.Date(origin)
  until <- as.Date(until)
  if (any(format(c(origin, until), "%d") != "01")) stop("Comparison windows must use calendar-month boundaries")
  horizon_months <- month_index(until) - month_index(origin)
  if (horizon_months < 1L || horizon_months > 12L) stop("Forecast horizons must be one to twelve months")
  validate_model_context()
  matches <- readr::read_csv("data/context/matches.csv", show_col_types = FALSE)
  split <- forecast_split(matches, origin, until, format)
  train <- split$train
  test <- split$test
  teams <- unique(c(train$team1_id, train$team2_id))
  known_team <- test$team1_id %in% teams & test$team2_id %in% teams
  split$coverage$known_team <- c(nrow(train), sum(known_team))
  test <- test[known_team, ]
  if (!nrow(test)) stop("No evaluation matches with established teams")
  output <- file.path("out/analysis/models", tolower(format), as.character(origin))
  replace_model_directory(output, function(output) {
    readr::write_csv(split$coverage, file.path(output, "coverage.csv"))
    message("Fitting ", format, ": ", nrow(train), " training matches, ", nrow(test), " evaluation matches")
    predictions <- list()
    counts <- tabulate(outcome_category(train$score), nbins = 3L) + 1
    draw <- counts[2] / sum(counts)
    predictions$climatology <- matrix(rep(c((1 - draw) / 2, draw, (1 - draw) / 2), each = nrow(test)), ncol = 3)
    recent <- train$start_date >= origin - 5L * 365L
    counts <- tabulate(outcome_category(train$score[recent]), nbins = 3L) + 1
    draw <- counts[2] / sum(counts)
    predictions$recent_climatology <- matrix(rep(c((1 - draw) / 2, draw, (1 - draw) / 2), each = nrow(test)), ncol = 3)
    predictions$recent_home_climatology <- predict_home_climatology(train[recent, ], test)
    message("Fitting Elo and Glicko-2 with past-only calibration")
    elo <- fit_elo_history(train, origin, horizon_months)
    elo_calibration <- fit_score_calibration(elo$predictor, train, origin)
    predictions$elo <- predict_score_calibration(elo_calibration, elo_predictor(elo$model, test, origin))
    elo_home <- fit_score_calibration(elo$predictor, train, origin, with_home = TRUE)
    predictions$elo_home <- predict_score_calibration(elo_home, elo_predictor(elo$model, test, origin), test$home_sign)
    glicko <- fit_glicko_history(train, origin, horizon_months)
    glicko_calibration <- fit_score_calibration(glicko$predictor, train, origin)
    glicko_x <- vapply(seq_len(nrow(test)), function(i) {
      glicko_predictor(glicko$status, test[i, ], month_index(test$start_date[i]) - month_index(origin))
    }, numeric(1))
    predictions$glicko2 <- predict_score_calibration(glicko_calibration, glicko_x)
    glicko_home <- fit_score_calibration(glicko$predictor, train, origin, with_home = TRUE)
    predictions$glicko2_home <- predict_score_calibration(glicko_home, glicko_x, test$home_sign)
    readr::write_csv(glicko$status, file.path(output, "glicko_ratings.csv"))
    readr::write_csv(elo_calibration$tuning, file.path(output, "elo_tuning.csv"))
    readr::write_csv(glicko_calibration$tuning, file.path(output, "glicko_tuning.csv"))
    readr::write_csv(elo_home$tuning, file.path(output, "elo_home_tuning.csv"))
    readr::write_csv(glicko_home$tuning, file.path(output, "glicko_home_tuning.csv"))
    message("Fitting paired team splines")
    spline <- select_spline(train, origin)
    predictions$gam <- predict_spline(spline$fit, test)
    readr::write_csv(spline$tuning, file.path(output, "gam_tuning.csv"))
    saveRDS(list(elo = elo, glicko = glicko, gam = spline), file.path(output, "fits.rds"))
    message("Fitting dynamic probit model in Stan")
    stan <- fit_dynamic_skill(train, test, file.path(output, "stan_draws"), iterations = iterations)
    export_model_strengths(spline$fit, stan, train, origin, output)
    predictions$stan_dynamic_probit <- stan$probabilities
    metrics <- do.call(rbind, lapply(names(predictions), function(name) {
      cbind(model = name, probability_metrics(predictions[[name]], test$score))
    }))
    metrics$stan_diagnostics_passed <- stan$diagnostics_passed
    readr::write_csv(metrics, file.path(output, "metrics.csv"))
    ledger <- do.call(rbind, lapply(names(predictions), function(name) {
      p <- predictions[[name]]
      data.frame(
        model = name, origin = origin,
        test[c("match_id", "format", "start_date", "team1", "team2", "score", "country")],
        p_loss = p[, 1], p_draw_tie = p[, 2], p_win = p[, 3]
      )
    }))
    readr::write_csv(ledger, file.path(output, "predictions.csv"))
    calibration <- do.call(rbind, lapply(names(predictions), function(name) {
      cbind(model = name, calibration_table(predictions[[name]], test$score))
    }))
    readr::write_csv(calibration, file.path(output, "calibration.csv"))
    sources <- c(
      list.files("R/models", pattern = "\\.R$", full.names = TRUE), "R/elo.R", "scripts/models.R",
      "models/dynamic_probit.stan", "renv.lock", "data/context/matches.csv"
    )
    outputs <- list.files(output, pattern = "\\.(csv|json)$", full.names = TRUE)
    outputs <- outputs[basename(outputs) != "manifest.json"]
    jsonlite::write_json(
      list(
        format = format, origin = origin, until = until, since = "2000-01-01", seed = 20261025L,
        train_ids = train$match_id, test_ids = test$match_id,
        hashes = setNames(lapply(sources, digest::digest, algo = "sha256", file = TRUE), sources),
        outputs = setNames(lapply(outputs, digest::digest, algo = "sha256", file = TRUE), basename(outputs)),
        cmdstan_version = as.character(cmdstanr::cmdstan_version()),
        glicko = list(init = c(2200, 300, 0.15), tau = 1.2, rdmax = 350, period = "calendar month"),
        iterations_per_chain = iterations, chains = 4, horizon_months = horizon_months,
        stan_diagnostics_passed = stan$diagnostics_passed,
        interpretation = "Exploratory frozen-origin experiment; not production ratings or independent validation"
      ), file.path(output, "manifest.json"),
      auto_unbox = TRUE, pretty = TRUE
    )
    print(metrics)
    if (!stan$diagnostics_passed) {
      cat(readLines(file.path(output, "stan_diagnostics.json")), sep = "\n")
      stop("Stan diagnostics failed; previous comparison has been preserved")
    }
    invisible(metrics)
  })
}
