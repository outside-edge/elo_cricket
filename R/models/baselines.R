glicko_empty_month <- function(status) {
  if (!is.null(status)) status$Lag[status$Games > 0] <- status$Lag[status$Games > 0] + 1L
  status
}

glicko_predictor <- function(status, matches, extra_months = 0L) {
  if (is.null(status)) {
    return(rep(0, nrow(matches)))
  }
  status$Deviation <- pmin(350, sqrt(
    status$Deviation^2 + (status$Lag + extra_months) * (status$Volatility * 400 / log(10))^2
  ))
  object <- structure(list(type = "Glicko-2", ratings = status), class = "rating")
  newdata <- data.frame(period = 1, a = matches$team1_id, b = matches$team2_id, score = 0)
  qlogis(predict(object, newdata = newdata, tng = 0, trat = c(2200, 300), gamma = 0))
}

fit_glicko_history <- function(train, origin, horizon_months = 12L) {
  if (format(origin, "%d") != "01") stop("Glicko origins must be calendar-month boundaries")
  periods <- seq(min(month_index(train$start_date)), month_index(origin) - 1L)
  status <- NULL
  forecast_status <- NULL
  forecast_period <- periods[1]
  predictor <- rep(NA_real_, nrow(train))
  for (period in periods) {
    if ((period - month_index(origin)) %% horizon_months == 0) {
      forecast_status <- status
      forecast_period <- period
    }
    starts <- month_index(train$start_date) == period
    if (any(starts)) predictor[starts] <- glicko_predictor(forecast_status, train[starts, ], period - forecast_period)
    ends <- month_index(train$end_date) == period
    if (any(ends)) {
      games <- data.frame(
        period = period, a = train$team1_id[ends], b = train$team2_id[ends], score = train$score[ends]
      )
      status <- PlayerRatings::glicko2(games, status = status, gamma = 0, sort = FALSE)$ratings
    } else {
      status <- glicko_empty_month(status)
    }
  }
  if (anyNA(predictor)) stop("Glicko history requires a calendar-month forecast origin")
  list(status = status, predictor = predictor, origin = origin)
}

elo_predictor <- function(model, matches, origin) {
  ratings <- elo_snapshot(model, origin - 1L)
  if (is.null(ratings)) {
    return(rep(0, nrow(matches)))
  }
  a <- ratings$rating[match(matches$team1_id, ratings$team_id)]
  b <- ratings$rating[match(matches$team2_id, ratings$team_id)]
  a[is.na(a)] <- 1000
  b[is.na(b)] <- 1000
  (a - b) / 400
}

fit_elo_history <- function(train, origin, horizon_months = 12L) {
  model <- fit_elo(train)
  predictor <- numeric(nrow(train))
  anchor <- month_index(origin)
  periods <- anchor + floor((month_index(train$start_date) - anchor) / horizon_months) * horizon_months
  for (period in unique(periods)) {
    rows <- periods == period
    predictor[rows] <- elo_predictor(model, train[rows, ], month_date(period))
  }
  list(model = model, predictor = predictor)
}

fit_score_calibration <- function(x, train, origin, with_home = FALSE) {
  inner <- previous_year(origin)
  recent <- train$start_date >= origin - 5L * 365L
  earlier <- train$end_date < inner & recent
  validation <- train$start_date >= inner
  if (sum(earlier) < 50 || !any(validation)) stop("Insufficient calibration data")
  fit <- function(rows, margin) {
    formula <- if (with_home) y ~ 0 + x + home + offset(shift) else y ~ 0 + x + offset(shift)
    mgcv::gam(
      formula,
      family = mgcv::ocat(theta = 2 * margin, R = 3), method = "REML",
      data = data.frame(
        y = outcome_category(train$score[rows]), x = x[rows], home = train$home_sign[rows], shift = margin - 1
      )
    )
  }
  grid <- draw_grid(unique(train$format))
  scores <- vapply(grid, function(margin) {
    model <- fit(earlier, margin)
    p <- predict(model, newdata = data.frame(
      x = x[validation], home = train$home_sign[validation], shift = margin - 1
    ), type = "response")
    probability_metrics(p, train$score[validation])$log_loss
  }, numeric(1))
  margin <- grid[which.min(scores)]
  list(model = fit(recent, margin), margin = margin, tuning = data.frame(margin = grid, log_loss = scores))
}

predict_score_calibration <- function(model, x, home = 0) {
  p <- predict(model$model, newdata = data.frame(x = x, home = home, shift = model$margin - 1), type = "response")
  check_probabilities(p)
  p
}

predict_home_climatology <- function(train, test) {
  home_score <- ifelse(train$home_sign == -1, 1 - train$score, train$score)
  counts <- tabulate(outcome_category(home_score[train$home_sign != 0]), nbins = 3L) + 1
  home_p <- counts / sum(counts)
  neutral <- tabulate(outcome_category(train$score[train$home_sign == 0]), nbins = 3L) + 1
  draw <- neutral[2] / sum(neutral)
  neutral_p <- c((1 - draw) / 2, draw, (1 - draw) / 2)
  p <- matrix(rep(neutral_p, each = nrow(test)), ncol = 3L)
  p[test$home_sign == 1, ] <- rep(home_p, each = sum(test$home_sign == 1))
  p[test$home_sign == -1, ] <- rep(rev(home_p), each = sum(test$home_sign == -1))
  check_probabilities(p)
  p
}
