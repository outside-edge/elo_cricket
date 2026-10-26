prepare_matches <- function(path = "data/cricket_matches_for_elo.csv") {
  cric <- read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  required <- c(
    "team1", "team2", "team1_id", "team2_id", "win_game",
    "date", "outcome", "match_id", "type_of_match"
  )
  if (!all(required %in% names(cric))) stop("Missing required match columns")
  cric <- cric[cric$type_of_match %in% c("Test", "ODI", "T20I"), ]
  if (anyDuplicated(cric$match_id)) stop("Duplicate match IDs")
  if (anyNA(cric[, required])) stop("Missing match fields")
  if (any(!nzchar(cric$team1) | !nzchar(cric$team2))) stop("Empty team names")
  if (any(cric$team1_id == cric$team2_id)) stop("A team cannot play itself")
  result <- tolower(cric$outcome)
  cric <- cric[!grepl("no result|abandoned|cancelled", result), ]
  cric$drawn <- grepl("drawn|tied", tolower(cric$outcome))
  valid_winner <- cric$win_game == cric$team1_id | cric$win_game == cric$team2_id
  if (any(!cric$drawn & !valid_winner)) stop("Unrecognized match outcome")
  cric$Date <- as.Date(cric$date, format = "%Y-%m-%d")
  if (anyNA(cric$Date)) stop("Invalid match dates")
  cric$winner <- ifelse(cric$win_game == cric$team1_id, cric$team1, cric$team2)
  cric$loser <- ifelse(cric$win_game == cric$team1_id, cric$team2, cric$team1)
  cric$score <- ifelse(cric$drawn, 0.5, as.numeric(cric$win_game == cric$team1_id))
  cric
}
