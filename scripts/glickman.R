# Glicko2 Algorithm

# Set dir.
setwd(githubdir)
setwd("elo_cricket/scripts/")

# Load libs
library(PlayerRatings)
data(aflodds)

# Ingest data
cric <- read.csv("../data/cricket_matches.csv")

# Recode
cric$Date   <- as.Date(cric$date, format =  "%Y-%m-%d")
cric$numdate <- as.numeric(cric$Date)
cric$winner <- as.character(cric$winner)
cric$loser  <- as.character(cric$loser)
cric$drawn  <- ifelse(is.na(cric$drawn), FALSE, cric$drawn == 1)

# Score w.r.t team 1
cric$score  <- ifelse(cric$winner == as.character(cric$team1), 1, 
	                 ifelse(cric$drawn == 1, .5, 0))

# Subset on Tests
cric_tests <- subset(cric, cric$type_of_match == "Test")

ratings <- glicko2(cric_tests[, c("numdate", "team1", "team2", "score")], history = TRUE)
teams_df   <- ratings[[1]]
teams        <- teams_df$Player[teams_df$Games > 10]
plot(ratings, players = teams)

rating_array <- ratings[[2]]
str(rating_array)

# As as many dfs as dates length(unique(cric_tests$numdate))
res <- data.frame(date = NA, teams = NA, ratings = NA)
for(i in 1:dim(ratings[[2]])[2]) {
	temp_ratings <- rating_array[, i, ][, 1]
	temp_res <- data.frame(date = i, teams = names(temp_ratings), ratings = temp_ratings)
	res <- rbind(res, temp_res)
}

small_res <- res[!(res$teams %in% c("Afghanistan", "ICC World XI")), ]

p2 = ggplot(small_res, aes(x = date, y = ratings, color = teams)) +
	 geom_smooth(method = "loess", se = F,  span = .1, size = .5)+ 
	 scale_color_brewer(palette = "Paired") +
	 ylab("Glicko 2 Ratings") + 
	 xlab("") + 
	 theme_minimal() + 
	 theme(legend.position = "none",
           plot.margin = unit(c(1,4,1,1), "lines")) +
	 geom_dl(aes(label = teams), method = "last.points", cex = 0.4) +
	 ggtitle("Smoothed Glicko 2 Ratings Over Time")

gt2 <- ggplotGrob(p2)
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"
grid.draw(gt2)
