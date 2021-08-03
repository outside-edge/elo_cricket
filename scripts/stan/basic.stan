data {
  int nteams;
  int ngames;
  int team1[ngames];
  int team2[ngames];
  vector[nteams] prior_score;
  vector[ngames] score1;
  vector[ngames] score2;
}

transformed data {
  vector[ngames] dif;
  dif = score1 - score2;
}

parameters {
  real b;
  real<lower=0> sigma_a;
  real<lower=0> sigma_y;
  vector[nteams] eta_a;
}

transformed parameters {
  vector[nteams] a;
  a = b*prior_score + sigma_a*eta_a;
}

model {
  eta_a ~ normal(0,1);
  for (i in 1:ngames)
    dif[i] ~ normal(a[team1[i]]-a[team2[i]], sigma_y);
}
