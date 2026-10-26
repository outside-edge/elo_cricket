source("R/models/common.R")
source("R/models/stan.R")

set.seed(613L)
n <- 800L
a <- sample(1:4, n, replace = TRUE)
b <- (a - 1L + sample(1:3, n, replace = TRUE)) %% 4L + 1L
venue <- sample(1:4, n, replace = TRUE)
time <- sort(sample(0:2920, n, replace = TRUE))
home <- as.integer(a == venue) - as.integer(b == venue)
strength <- c(-0.9, -0.3, 0.3, 0.9)
eta <- strength[a] - strength[b] + 0.5 * home
margin <- exp(log(0.4) - 0.07 * time / 365.25)
performance <- eta + rnorm(n)
teams <- c("England", "India", "Australia", "South Africa")
matches <- data.frame(
  match_id = seq_len(n), format = "Test", start_date = as.Date("2000-01-01") + time,
  end_date = as.Date("2000-01-01") + time + 4L,
  team1_id = a, team2_id = b, team1 = teams[a], team2 = teams[b],
  score = ifelse(performance < -margin, 0, ifelse(performance > margin, 1, 0.5)),
  country = teams[venue], ground_profile_id = venue, home_sign = home
)
train <- matches[1:700, ]
test <- matches[701:800, ]
output <- "out/analysis/models/simulation"
result <- fit_dynamic_skill(train, test, file.path(output, "stan_draws"))
stopifnot(result$diagnostics_passed)
summary <- result$fit$summary(variables = c("home_mean", "draw_trend", "sigma_walk"))
home_summary <- summary[summary$variable == "home_mean", ]
stopifnot(home_summary$q5 < 0.5, home_summary$q95 > 0.5, home_summary$mean > 0)
draw_summary <- summary[summary$variable == "draw_trend", ]
stopifnot(draw_summary$q5 < -0.07, draw_summary$q95 > -0.07, draw_summary$mean < 0)
skill <- result$fit$summary(variables = "skill")
last_quarter <- max(result$inputs$data$quarter[seq_len(nrow(train))])
last <- skill[match(paste0("skill[", 1:4, ",", last_quarter, "]"), skill$variable), ]
stopifnot(all(diff(last$mean) > 0), all(last$q5 < strength), all(last$q95 > strength))
swapped <- stan_inputs(train, swap_teams(test))
model <- compile_skill_model()
generated <- model$generate_quantities(
  data = swapped$data, fitted_params = result$fit$output_files(), seed = 315L,
  parallel_chains = 4, output_dir = file.path(output, "stan_draws")
)
original_p <- result$fit$summary(variables = "p")
swapped_p <- generated$summary(variables = "p")
reverse_names <- sub(",1]", ",X]", original_p$variable, fixed = TRUE)
reverse_names <- sub(",3]", ",1]", reverse_names, fixed = TRUE)
reverse_names <- sub(",X]", ",3]", reverse_names, fixed = TRUE)
stopifnot(max(abs(original_p$mean - swapped_p$mean[match(reverse_names, swapped_p$variable)])) < 1e-7)
readr::write_csv(summary, file.path(output, "recovery.csv"))
cat("Stan simulation recovery, diagnostics, and generated-quantities swap checks passed.\n")
