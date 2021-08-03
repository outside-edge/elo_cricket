# Adapted From Andrew Gelman's Stan Goes To Worldcup Code
# https://statmodeling.stat.columbia.edu/2014/07/13/stan-analyzes-world-cup-data/
# Worldcup stan gives nonsense results
# 

# Set dir.
setwd(githubdir)
setwd("elo_cricket/scripts/")

# Load lib.
library(rstan)
library(arm)
library(coefplot)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## Sebastian's function to run stan with caching of compiled Stan models
stan_run <- function(stanModel, ...) {
  if(class(stanModel) == "stanfit") {
    stanExe <- stanModel
  } else {
    stanModel.rda <- gsub("stan$", "rda", stanModel)
    if(!file.exists(stanModel.rda) || file.info(stanModel.rda)$mtime < file.info(stanModel)$mtime) {
      cat("Model",stanModel,"needs recompilation.\n")
      args <- modifyList(list(...), list(file=stanModel, iter=0, warmup=0, chains=0))
      stanExe <- do.call(stan, args)
      saveRDS(stanExe, file=stanModel.rda)
    } else {
      cat("Loading cached stan model", stanModel, ".\n")
      stanExe = readRDS(stanModel.rda)
    }
  }
  # This bit with the seed is for debugging purposes; once we figure out why Stan is crashing R we can remove it.
  seed <- sample.int(.Machine$integer.max, 1)
  write (seed, file="stan_seed.txt")
  stan(fit=stanExe, seed=seed, ...)
}

# Ingest data
cric <- read.csv("../data/cricket_matches.csv")

cric$drawn        <- grepl("drawn", tolower(cric$outcome))
cric$team1_score  <- ifelse(cric$win_game == cric$team1_id, 1, ifelse(cric$drawn == 1, .5, 0))
cric$team2_score  <- ifelse(cric$win_game == cric$team2_id, 1, ifelse(cric$drawn == 1, .5, 0))

cric$team1_ump1    <- 
cric$team1_ump2    <- 
cric$team2_ump1    <- 
cric$team2_ump2    <- ifelse(cric$umpire_2_country == cric$team2, 1, 0)

# Subset on Tests
cric_tests <- subset(cric, cric$type_of_match == "Test")

teams  <- unique(c(cric_tests$team1, cric_tests$team2))
nteams <- length(teams)
prior_score <- rev(1:nteams)
prior_score <- (prior_score - mean(prior_score))/(2*sd(prior_score))

ngames   <- nrow(cric_tests)

team1  <- match(cric_tests$team1, teams)
score1 <- cric_tests$team1_score
team2  <- match(cric_tests$team2, teams)
score2 <- cric_tests$team2_score
ump1team1 <- ifelse(cric$umpire_1_country == cric$team1, 1, 0)
ump2team1 <- ifelse(cric$umpire_2_country == cric$team1, 1, 0)
hometeam1 <- 1*(cric_tests$team1 == cric_tests$umpire_1_country) 
tossteam1 <- 1*(cric_tests$team1_id == cric_tests$win_toss)

df <- 9

data <- c("nteams", "ngames", "team1", "score1", "team2", "score2", "prior_score", "df")

fit <- stan_run("stan/basic.stan", data = data, chains = 4, iter = 10000)
print(fit)

colVars <- function(a) {
  n <- dim(a)[[1]]; 
  c <- dim(a)[[2]]; 
  return(.colMeans(((a - matrix(.colMeans(a, n, c), nrow = n, ncol = c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))
}

sims   <- rstan::extract(fit)
a_sims <- sims$a
a_hat  <- colMeans(a_sims)
a_se   <- sqrt(colVars(a_sims))

res <- data.frame(a_hat = a_hat, a_se = a_se, teams = teams)

ggplot(res, aes(a_hat, a_se)) + 
  geom_text(aes(label = teams), size=3) + 
  theme_minimal()
ggsave("../figs/all_time_cric_tests_stan.png")

# Add Team1 Toss, Team1 Home + Team1 Umpire
# Move to normal
data <- c("nteams", "ngames", "team1", "score1", "team2", "score2", "prior_score",
          "ump1team1", "ump2team1", "hometeam1", "tossteam1")

fit <- stan_run("stan/basic_plus.stan", data = data, chains = 4, iter = 1000)
print(fit)

sims   <- rstan::extract(fit)
a_sims <- sims$a
a_hat  <- colMeans(a_sims)
a_se   <- sqrt(colVars(a_sims))

res <- data.frame(a_hat = a_hat, a_se = a_se, teams = teams)

ggplot(res, aes(a_hat, a_se)) + 
  geom_text(aes(label = teams), size=3) + 
  theme_minimal()
ggsave("../figs/all_time_cric_tests_with_covar_stan.png")

# Let's try to regress out umpire, toss, home/away and then estimate ability to verify

summary(lm(I(score1 - score2) ~ ump1team1 + ump2team1 + hometeam1 + tossteam1))

# Let's add random effects by time
data <- c("nteams", "ngames", "team1", "score1", "team2", "score2", "prior_score", "df", 
          "ump1team1", "ump2team1", "hometeam1", "tossteam1", "time")
fit <- stan_run("stan/basic_plus_time.stan", data = data, chains = 4, iter = 100)
print(fit)

# Let's do a dynamic linear model

# Let's now try to learn from two different ys

fit_noprior <- stan_run("stan/noprior_matt.stan", data=data, chains=4, iter=500)
print(fit_noprior)
fit_noprior <- stan_run("stan/noprior_matt.stan", data=data, chains=4, iter=1000)
print(fit_noprior)

worldcup_plot <- function (fit, file.png){
  sims <- extract(fit)
  a_sims <- sims$a
  a_hat <- colMeans(a_sims)
  a_se <- sqrt(colVars(a_sims))
  png (file.png, height=500, width=500)
  coefplot (rev(a_hat), rev(a_se), CI=1, varnames=rev(teams), main="Team quality (estimate +/- 1 s.e.)\n", cex.var=.9, mar=c(0,4,5.1,2), xlim=c(-.5,.5))
  dev.off()
}
worldcup_plot(fit_noprior, "worldcup1_noprior.png")

sims <- extract (fit)
a_sims <- sims$a
a_hat <- colMeans(a_sims)
a_se <- sqrt(colVars(a_sims))
expected_on_sqrt_scale <- a_hat[team1] - a_hat[team2]
sigma_y_sims <- sims$sigma_y
interval_975 <- median(qt(.975,df)*sigma_y_sims)
signed_square <- function (a) {sign(a)*a^2}
lower <- signed_square(expected_on_sqrt_scale - interval_975)
upper <- signed_square(expected_on_sqrt_scale + interval_975)

png ("worldcup2.png", height=1000, width=500)
coefplot (rev(score1 - score2), sds=rep(0, ngames),
          lower.conf.bounds=rev(lower), upper.conf.bounds=rev(upper), 
          varnames=rev(paste(teams[team1], "vs.", teams[team2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

fit <- stan_run("worldcup_matt.stan", data=data, chains=4, iter=5000)
print(fit)

sims <- extract (fit)
a_sims <- sims$a
sigma_y_sims <- sims$sigma_y
nsims <- length(sigma_y_sims)
random_outcome <- array(NA, c(nsims,ngames))
for (s in 1:nsims){
  random_outcome_on_sqrt_scale <- (a_sims[s,team1] - a_sims[s,team2]) + rt(ngames,df)*sigma_y_sims[s]
  random_outcome[s,] <- signed_square(random_outcome_on_sqrt_scale)
}
sim_quantiles <- array(NA,c(ngames,2))
for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(random_outcome[,i], c(.025,.975))
}

png ("worldcup3.png", height=1000, width=500)
coefplot (rev(score1 - score2), sds=rep(0, ngames),
          lower.conf.bounds=rev(sim_quantiles[,1]), upper.conf.bounds=rev(sim_quantiles[,2]), 
          varnames=rev(paste(teams[team1], "vs.", teams[team2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

# Do it again, rounding the continuous predictions:

for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(round(random_outcome[,i]), c(.025,.975))
}

png ("worldcup4.png", height=1000, width=500)
coefplot (rev(score1 - score2), sds=rep(0, ngames),
          lower.conf.bounds=rev(sim_quantiles[,1]), upper.conf.bounds=rev(sim_quantiles[,2]), 
          varnames=rev(paste(teams[team1], "vs.", teams[team2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

# Reorder the games in order of predicted score differential
new_order <- order(prior_score[team1] - prior_score[team2])

for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(random_outcome[,i], c(.025,.975))
}

png ("worldcup5.png", height=1000, width=500)
coefplot ((score1 - score2)[new_order], sds=rep(0, ngames),
          lower.conf.bounds=sim_quantiles[new_order,1], upper.conf.bounds=sim_quantiles[new_order,2], 
          varnames=paste(teams[team1[new_order]], "vs.", teams[team2[new_order]]),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

# Flip so expected outocmes are always positive
flip <- ifelse (prior_score[team1] < prior_score[team2], -1, 1)
new_order <- order((prior_score[team1] - prior_score[team2])*flip)
flip <- flip[new_order]

png ("worldcup6.png", height=1000, width=500)
coefplot ((score1 - score2)[new_order]*flip, sds=rep(0, ngames),
          lower.conf.bounds=sim_quantiles[new_order,1]*flip, upper.conf.bounds=sim_quantiles[new_order,2]*flip, 
          varnames=ifelse(flip==1, paste(teams[team1[new_order]], "vs.", teams[team2[new_order]]),
                          paste(teams[team2[new_order]], "vs.", teams[team1[new_order]])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()


