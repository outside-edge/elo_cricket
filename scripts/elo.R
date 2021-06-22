## ELO Rankings

setwd(githubdir)
setwd("elo_cricket/scripts/")

# load packages
library(readr)
library(EloRating)

# Ingest data
cric <- read_csv("../data/cricket_matches.csv")

# Recode
cric$Date   <- as.Date(cric$date, format =  "%m/%d/%Y")
cric$winner <- as.character(cric$winner)
cric$loser  <- as.character(cric$loser)
cric$drawn  <- ifelse(is.na(cric$drawn), FALSE, cric$drawn == 1)

cric$loser  <- ifelse(is.na(cric$loser), 
					  ifelse(cric$winner == as.character(cric$team1_id),
					  as.character(cric$team2_id),
					  as.character(cric$team1_id)),
					  cric$loser) # For EloRating -- no NAs in loserre

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
extract_elo(res, extractdate = "2000-05-28")
