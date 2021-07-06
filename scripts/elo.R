## ELO Rankings
# cribbing from: https://cran.r-project.org/web/packages/EloRating/vignettes/EloRating_tutorial.pdf

setwd(githubdir)
setwd("elo_cricket/scripts/")

# load packages
library(readr)
library(EloRating)
library(ggplot2)
library(tidyverse)
library(grid)
library(directlabels)

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

test_res <- with(cric_tests, elo.seq(winner = winner,
	                      loser  = loser,
	                      draw   = drawn,
	                      Date   = Date))

summary(test_res)

# Latest Rankings
latest_ratings          <- extract_elo(test_res) # provides ratings for the latest time period
latest_ratings_df       <- as.data.frame(latest_ratings)
latest_ratings_df$teams <- as.factor(rownames(latest_ratings_df))

latest_ratings_df %>%
  mutate(teams = fct_reorder(teams, desc(latest_ratings))) %>%
  ggplot(aes(x = latest_ratings, y = teams)) + 
	geom_point() +
	theme_minimal() + 
	ylab("") + 
	xlab("Test Ratings (6/18/2021)")
ggsave("../figs/test_ratings_2021-06-18.png")
dev.off()

# Plot the entire time series

eloplot(eloobject = test_res,
	    ids = union(cric_tests$team1, cric_tests$team2))
dev.off()

# Let's get average ratings per month
# Iterate over all the months
# Output is Long DF
test_res_df <- data.frame(teams = NULL, date = NULL, m_ratings = NULL)
min_year <- format(as.POSIXct(test_res$misc["minDate"], format = "%Y-%m-%d"), format = "%Y")
max_year <- format(as.POSIXct(test_res$misc["maxDate"], format = "%Y-%m-%d"), format = "%Y")
month_ts <- seq(as.Date(test_res$misc["minDate"]), as.Date(test_res$misc["maxDate"]), by = "month")

for(i in month_ts[1:length(month_ts)-1]) {
	m_ratings <- extract_elo(test_res, 
		        NA.interpolate = T, 
		        extractdate	= as.Date(i),
		        daterange = 30) # We are smoothing ratings by year
	m_ratings_df <- data.frame(m_ratings, teams = names(m_ratings), date = as.Date(i), row.names = NULL)
	test_res_df <- rbind(test_res_df, m_ratings_df)
}

write_csv(test_res_df, file = "../data/test_ratings_1881_2021.csv")

small_test_df <- test_res_df[!(test_res_df$teams %in% c("Afghanistan", "ICC World XI")), ]

p2 = ggplot(small_test_df, aes(x = date, y = m_ratings, color = teams)) +
	geom_smooth(method = "loess", se = F,  span = .1, size = .5)+ 
	scale_color_brewer(palette = "Paired") +
	ylab("ELO Ratings") + 
	xlab("") + 
	scale_x_continuous(breaks = seq(min(month_ts), max(month_ts) + 10, length.out = 15),
					   labels = format(seq(min(month_ts), max(month_ts), length.out = 15), "%Y")) + 
	scale_y_continuous(breaks = round(seq(min(small_test_df$m_ratings, na.rm = T), max(small_test_df$m_ratings, na.rm = T), by = 100), digits = -1),
                       labels = round(seq(min(small_test_df$m_ratings, na.rm = T), max(small_test_df$m_ratings, na.rm = T), by = 100), digits = -1)) + 
	theme_minimal() + 
	theme(legend.position = "none",
          plot.margin = unit(c(1,4,1,1), "lines")) +
	geom_dl(aes(label = teams), method = "last.points", cex = 0.4) +
	ggtitle("Smoothed ELO Ratings Over Time")

gt2 <- ggplotGrob(p2)
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"
grid.draw(gt2)
ggsave("../figs/test_ratings_1881_2021.png", plot = gt2)

# ODIs
# Subset on ODIs
cric_odis <- subset(cric, cric$type_of_match == "ODI")
cric_odis <- cric_odis[order(cric_odis$Date), ] # for EloRating

with(cric_odis, seqcheck(winner = winner,
	                      loser  = loser,
	                      draw   = drawn,
	                      Date   = Date))

odi_res <- with(cric_odis, elo.seq(winner = winner,
	                      loser  = loser,
	                      draw   = drawn,
	                      Date   = Date))

summary(odi_res)

# Let's get average ratings per month
# Iterate over all the months
# Output is Long DF
odi_res_df <- data.frame(teams = NULL, date = NULL, m_ratings = NULL)
min_year <- format(as.POSIXct(odi_res$misc["minDate"], format = "%Y-%m-%d"), format = "%Y")
max_year <- format(as.POSIXct(odi_res$misc["maxDate"], format = "%Y-%m-%d"), format = "%Y")
month_ts <- seq(as.Date(odi_res$misc["minDate"]), as.Date(odi_res$misc["maxDate"]), by = "month")

for(i in month_ts[1:length(month_ts)-1]) {
	m_ratings <- extract_elo(odi_res, 
		        NA.interpolate = T, 
		        extractdate	= as.Date(i),
		        daterange = 30) # We are smoothing ratings by year
	m_ratings_df <- data.frame(m_ratings, teams = names(m_ratings), date = as.Date(i), row.names = NULL)
	odi_res_df <- rbind(odi_res_df, m_ratings_df)
}

write_csv(odi_res_df, file = "../data/odi_ratings_1972_2021.csv")

# Only keeping teams with more than 100 matches
odi_teams <- table(c(cric_odis$team1, cric_odis$team2))
small_odi_df <- odi_res_df[(odi_res_df$teams %in%  names(odi_teams[odi_teams > 200])), ]

p2 = ggplot(small_odi_df, aes(x = date, y = m_ratings, color = teams)) +
	geom_smooth(method = "loess", se = F,  span = .1, size = .5)+ 
	scale_color_brewer(palette = "Paired") +
	ylab("ELO Ratings") + 
	xlab("") + 
	scale_x_continuous(breaks = seq(min(month_ts), max(month_ts) + 10, length.out = 15),
					   labels = format(seq(min(month_ts), max(month_ts), length.out = 15), "%Y")) + 
	scale_y_continuous(breaks = round(seq(min(small_odi_df$m_ratings, na.rm = T), max(small_odi_df$m_ratings, na.rm = T), by = 100), digits = -1),
                       labels = round(seq(min(small_odi_df$m_ratings, na.rm = T), max(small_odi_df$m_ratings, na.rm = T), by = 100), digits = -1)) + 
	theme_minimal() + 
	theme(legend.position = "none",
          plot.margin = unit(c(1,4,1,1), "lines")) +
	geom_dl(aes(label = teams), method = list("last.points", cex = 0.6, vjust = 0)) +
	ggtitle("Smoothed ELO Ratings Over Time")

gt2 <- ggplotGrob(p2)
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"
grid.draw(gt2)
ggsave("../figs/odi_ratings_1972_2021.png", plot = gt2)
