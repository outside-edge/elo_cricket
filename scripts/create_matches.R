library(readr)
library(dplyr)

#setwd("/Users/derekwillis/code/cricket-stats")

matches <- read_csv("data/final_output.csv")

elo_input <- matches %>%
select(match_id, win_game, team1_id, team1, team2_id, team2, date, type_of_match, outcome)

winner <- elo_input %>%
mutate(winner= case_when(win_game == team1_id ~ team1_id, win_game == team2_id ~ team2_id, win_game == 0 ~ 0))

loser <- winner %>%
mutate(loser= case_when(win_game == team1_id ~ team2_id, win_game == team2_id ~ team1_id, win_game == 0 ~ 0))

drawn <- loser %>%
mutate(drawn = if_else(outcome == 'Match drawn', 1, 0))

tied <- drawn %>%
mutate(tied = if_else(outcome == 'Match tied', 1, 0))

cricket_matches <- tied
write_excel_csv(cricket_matches, "cricket_matches.csv")
