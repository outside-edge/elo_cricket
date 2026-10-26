root <- normalizePath("../..")
for (module in list.files(file.path(root, "R/models"), pattern = "\\.R$", full.names = TRUE)) source(module)

simulated_matches <- function(n = 600L, seed = 81L) {
  set.seed(seed)
  a <- sample(seq_len(4), n, replace = TRUE)
  b <- (a - 1L + sample(1:3, n, replace = TRUE)) %% 4L + 1L
  time <- sort(sample(seq_len(3650), n, replace = TRUE))
  countries <- c("England", "India", "Australia", "South Africa")
  venue <- sample(seq_len(4), n, replace = TRUE)
  home <- as.integer(a == venue) - as.integer(b == venue)
  strengths <- c(-1.5, -0.5, 0.5, 1.5)
  eta <- strengths[a] - strengths[b] + 0.6 * home
  performance <- eta + rlogis(n)
  score <- ifelse(performance < -0.5, 0, ifelse(performance > 0.5, 1, 0.5))
  data.frame(
    match_id = seq_len(n), format = "Test", start_date = as.Date("2000-01-01") + time,
    end_date = as.Date("2000-01-01") + time + 4L,
    team1_id = a, team2_id = b, team1 = countries[a], team2 = countries[b],
    score = score, result = ifelse(score == 0.5, "drawn", "win"),
    country = countries[venue], ground_profile_id = venue * 10L + sample(1:2, n, replace = TRUE),
    home_sign = home
  )
}
