# Glicko2 Algorithm

# Set dir.
setwd(githubdir)
setwd("elo_cricket/")

# Load libs
library(PlayerRatings)
library(ggplot2)
library(tidyverse)
library(directlabels)

# Ingest data
cric <- read.csv("data/cricket_matches.csv")

# Recode
cric$Date    <- as.Date(cric$date, format =  "%Y-%m-%d")

cric$drawn  <- grepl("drawn", tolower(cric$outcome))
cric$loser  <- ifelse(cric$win_game == cric$team1_id, cric$team2, cric$team1) 
cric$winner <- ifelse(cric$win_game == cric$team1_id, cric$team1, cric$team2) 

# Score w.r.t team 1
cric$score  <- ifelse(cric$win_game == cric$team1_id, 1, ifelse(cric$drawn == 1, .5, 0))

# Subset on Tests
cric_tests <- subset(cric, cric$type_of_match == "Test")
cric_tests$ord_date <- rank(cric_tests$Date, ties = "min")
cric_tests_o <- cric_tests[order(cric_tests$Date), ]

cric_tests_o$newdat <- NA

diffs <- c(1, diff(cric_tests_o$ord_date))

j = 0

for (i in 1:nrow(cric_tests_o)){

	if (diffs[i] == 1){
		j = j + 2 - diffs[i]
	}

	if (diffs[i] == 2){
		j = j + 3 - diffs[i]
	}

	if (diffs[i] == 3){
		j = j + 4 - diffs[i]
	}

	cric_tests_o$newdat[i] = j
}


ratings <- glicko2(cric_tests[, c("ord_date", "team1", "team2", "score")], 
	               history = TRUE, 
	               tau = .6)

#teams_df   <- ratings[[1]]
#teams      <- teams_df$Player[teams_df$Games > 10]
#plot(ratings, players = teams)
#dev.off()

rating_array <- ratings[[2]]
str(rating_array)

# As as many dfs as dates length(unique(cric_tests$numdate))
res <- data.frame(date = NULL, teams = NULL, ratings = NULL)
for(i in 1:dim(ratings[[2]])[2]) {
	temp_ratings <- rating_array[, i, ][, 1]
	temp_res <- data.frame(date = i, teams = names(temp_ratings), ratings = temp_ratings)
	res <- rbind(res, temp_res)
}

# Get the real date
res_date <- res %>% 
            left_join(cric_tests_o[, c("Date", "newdat")], by = c("date" = "newdat"))

# Write output
write.csv(res_date, file = "data/glicko2_test_ratings_1881_2021.csv")

# Subset
small_res <- res_date[!(res_date$teams %in% c("Afghanistan", "ICC World XI")), ]
small_res[small_res$date == max(small_res$date), ]


gg = ggplot(small_res, aes(x = Date, y = ratings, color = teams)) +
	 geom_smooth(method = "loess", se = F,  span = .1, size = .5)+ 
	 scale_color_brewer(palette = "Paired") +
	 ylab("Glicko 2 Ratings") + 
	 xlab("") + 
	 theme_minimal() + 
	 theme(legend.position = "none",
           plot.margin = unit(c(1,2,3,2), "lines")) + 
	   scale_x_continuous(breaks = seq(min(small_res_date$Date, na.rm = T), max(small_res_date$Date + 500, na.rm = T), by = 3650), 
                     limits = c(min(small_res_date$Date, na.rm = T), max(small_res_date$Date + 500, na.rm = T)))

direct.label(gg, method = list(box.color = NA, "angled.boxes"))
ggsave("figs/test_ratings_glicko2_2021.png")

ggplot(small_res_date, aes(x = Date, y = ratings, color = teams)) +
	 geom_smooth(method = "gam", se = F,  span = .1, size = .5)+ 
	 scale_color_brewer(palette = "Paired") + 
	 geom_text(data = . %>%  
              group_by(teams) %>%  
              drop_na() %>%  
              filter(Date == max(Date)),  
            aes(label = teams),  
            nudge_x = .1,  
            nudge_y = .12,  
            color = "black",  
            size = 3) +  
# give them a cute little end point
  geom_point(data = . %>%  
              group_by(teams) %>%  
              drop_na() %>%  
              filter(Date == max(Date)),  
            aes(fill = teams),  
            shape = 21,  
            size = 2,  
            color = "white") +  
# but probably wider x axis
  scale_x_continuous(breaks = seq(min(small_res_date$Date, na.rm = T), max(small_res_date$Date + 500, na.rm = T), by = 3650), 
                     limits = c(min(small_res_date$Date, na.rm = T), max(small_res_date$Date + 500, na.rm = T))) +  
  scale_y_continuous(expand = c(0,.15)) +  
  labs(x = NULL, y = NULL) +  
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(panel.grid.minor.x = element_blank()) +  
  theme(panel.grid.major.x = element_blank()) +  
  theme(plot.title.position = "plot") +  
  theme(plot.background = element_rect(fill = "grey98")) +  
  theme(axis.line.x = element_line(color = "grey20")) +  
  theme(axis.ticks.x = element_line(color = "grey20")) +  
  labs(title = "")
