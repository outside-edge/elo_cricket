stan_inputs <- function(train, test, prior_only = FALSE) {
  matches <- rbind(train, test)
  levels <- list(
    team = sort(unique(c(matches$team1_id, matches$team2_id))),
    country = sort(unique(matches$country)), ground = sort(unique(matches$ground_profile_id)),
    quarter = seq(min(quarter_index(matches$start_date)), max(quarter_index(matches$start_date)))
  )
  if (anyNA(matches$country) || anyNA(matches$home_sign)) stop("Stan requires known venue countries")
  data <- list(
    N = nrow(matches), N_train = nrow(train), J = length(levels$team), Q = length(levels$quarter),
    C = length(levels$country), G = length(levels$ground),
    a = match(matches$team1_id, levels$team), b = match(matches$team2_id, levels$team),
    quarter = match(quarter_index(matches$start_date), levels$quarter),
    country = match(matches$country, levels$country), ground = match(matches$ground_profile_id, levels$ground),
    home = matches$home_sign,
    era = as.numeric(matches$start_date - min(train$start_date)) / 365.25,
    y = outcome_category(train$score), draw_prior = if (unique(train$format) == "Test") log(0.3) else log(0.01),
    prior_only = as.integer(prior_only)
  )
  list(data = data, levels = levels)
}

compile_skill_model <- function() {
  local <- "renv/local/cmdstan/cmdstan-2.40.0"
  if (!nzchar(Sys.getenv("CMDSTAN")) && dir.exists(local)) cmdstanr::set_cmdstan_path(local)
  dir.create("renv/local/models", recursive = TRUE, showWarnings = FALSE)
  source <- "models/dynamic_probit.stan"
  target <- "renv/local/models/dynamic_probit.stan"
  if (!file.exists(target) || digest::digest(file = source) != digest::digest(file = target)) {
    file.copy(source, target, overwrite = TRUE)
  }
  cmdstanr::cmdstan_model(target, quiet = TRUE, stanc_options = list("warn-pedantic" = TRUE))
}

fit_dynamic_skill <- function(train, test, output_dir, seed = 20261025L, iterations = 1000L, prior_only = FALSE) {
  inputs <- stan_inputs(train, test, prior_only)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(inputs$levels, file.path(dirname(output_dir), "stan_levels.json"), pretty = TRUE)
  model <- compile_skill_model()
  init <- function() {
    list(
      z_skill = matrix(rnorm(inputs$data$J * inputs$data$Q, sd = 0.1), inputs$data$J, inputs$data$Q),
      sigma_walk = 0.1, home_mean = 0, z_draw_trend = 0, log_draw = inputs$data$draw_prior,
      sigma_country_home = 0.1, sigma_ground_home = 0.1, sigma_country_draw = 0.1, sigma_ground_draw = 0.1,
      z_country_home = rep(0, inputs$data$C), z_ground_home = rep(0, inputs$data$G),
      z_country_draw = rep(0, inputs$data$C), z_ground_draw = rep(0, inputs$data$G)
    )
  }
  set.seed(seed)
  fit <- model$sample(
    data = inputs$data, seed = seed, chains = 4, parallel_chains = 4,
    iter_warmup = iterations, iter_sampling = iterations, adapt_delta = 0.95,
    max_treedepth = 12, refresh = 200, output_dir = output_dir, init = init, step_size = 0.05, sig_figs = 12
  )
  diagnostics <- fit$diagnostic_summary()
  parameters <- c(
    "sigma_walk", "home_mean", "sigma_country_home", "sigma_ground_home", "log_draw", "draw_trend",
    "sigma_country_draw", "sigma_ground_draw", "skill", "country_home", "ground_home", "country_draw", "ground_draw"
  )
  summary <- fit$summary(variables = parameters)
  passed <- all(diagnostics$num_divergent == 0 & diagnostics$num_max_treedepth == 0 & diagnostics$ebfmi > 0.3) &&
    all(is.finite(summary$rhat) & summary$rhat < 1.01 & summary$ess_bulk >= 400 & summary$ess_tail >= 400)
  jsonlite::write_json(
    list(
      passed = passed, sampler = diagnostics, max_rhat = max(summary$rhat),
      min_bulk_ess = min(summary$ess_bulk), min_tail_ess = min(summary$ess_tail)
    ),
    file.path(dirname(output_dir), "stan_diagnostics.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
  readr::write_csv(summary, file.path(dirname(output_dir), "stan_parameters.csv"))
  p_summary <- fit$summary(variables = "p")
  p <- matrix(NA_real_, nrow(test), 3L)
  indices <- strcapture("p\\[([0-9]+),([0-9]+)\\]", p_summary$variable, list(row = integer(), col = integer()))
  p[as.matrix(indices)] <- p_summary$mean
  check_probabilities(p)
  ppc <- fit$summary(variables = "replicated_counts")
  ppc$observed <- tabulate(outcome_category(train$score), nbins = 3L)
  readr::write_csv(ppc, file.path(dirname(output_dir), "stan_predictive_checks.csv"))
  list(fit = fit, inputs = inputs, probabilities = p, diagnostics_passed = passed)
}
