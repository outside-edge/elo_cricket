library(tidyverse)
library(readr)
cricket_matches <- read_csv("~/code/elo_cricket/data/cricket_matches.csv")
cricket_matches <- mutate(cricket_matches, winner=case_when(win_game==team1_id ~ team1_id, win_game==team2_id ~ team2_id, win_game==0 ~ 0)) %>%
  mutate(cricket_matches, loser=case_when(win_game==team2_id ~ team1_id, win_game==team1_id ~ team2_id, win_game==0 ~ 0)) %>%
  mutate(cricket_matches, drawn=ifelse(str_detect(outcome, 'drawn'), 1, 0)) %>%
  mutate(cricket_matches, tied=ifelse(str_detect(outcome, 'tied'), 1, 0))
write_excel_csv(cricket_matches, "cricket_matches_for_elo.csv")