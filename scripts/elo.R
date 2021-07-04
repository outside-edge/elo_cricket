## ELO Rankings
# cribbing from: https://cran.r-project.org/web/packages/EloRating/vignettes/EloRating_tutorial.pdf

setwd(githubdir)
setwd("elo_cricket/scripts/")

# load packages
library(readr)
library(EloRating)
library(ggplot2)
library(tidyverse)

# Ingest data
cric <- read.csv("../data/cricket_matches.csv")

# Recode
cric$Date   <- as.Date(cric$date, format =  "%Y-%m-%d")
cric$winner <- as.character(cric$winner)
cric$loser  <- as.character(cric$loser)
cric$drawn  <- ifelse(is.na(cric$drawn), FALSE, cric$drawn == 1)

cric$loser  <- ifelse(cric$loser == "", 
					  ifelse(cric$winner == as.character(cric$team1_id),
					  as.character(cric$team2),
					  as.character(cric$team1)),
					  cric$loser) # For EloRating -- no NAs in loserre

cric$winner  <- ifelse(cric$winner == "", 
					  ifelse(cric$winner == as.character(cric$team1_id),
					  as.character(cric$team1),
					  as.character(cric$team2)),
					  cric$winner) # For EloRating -- no NAs in winner

# Subset on Tests
cric_tests <- subset(cric, cric$type_of_match =="Test")
cric_tests <- cric_tests[order(cric_tests$Date), ] # for EloRating

with(cric_tests, seqcheck(winner = winner,
	                      loser  = loser,
	                      draw   = drawn,
	                      Date   = Date))

res <- with(cric_tests, elo.seq(winner = winner,
	                      loser  = loser,
	                      draw   = drawn,
	                      Date   = Date))

summary(res)

# Validation
latest_rankings          <- extract_elo(res) # provides ratings for the latest time period
latest_rankings_df       <- as.data.frame(latest_rankings)
latest_rankings_df$teams <- as.factor(rownames(latest_rankings_df))

latest_rankings_df %>%
  mutate(teams = fct_reorder(teams, desc(latest_rankings))) %>%
  ggplot(aes(x = latest_rankings, y = teams)) + 
	geom_point() +
	theme_minimal() + 
	ylab("") + 
	xlab("Test Rankings as of ")
ggsave("../figs/test_rankings_2021-06-18.png")

eloplot(eloobject = res,  from = "2000-06-05", to = "2005-07-04")
