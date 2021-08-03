data {
  int nteams;
  int ngames;
  vector[nteams] prior_score;
  int team1[ngames];
  int team2[ngames];
  vector[ngames] score1;
  vector[ngames] score2;
  vector[ngames] ump1team1;
  vector[ngames] ump2team1;
  vector[ngames] tossteam1;
  vector[ngames] hometeam1;
}

transformed data {
  vector[ngames] dif;
  dif = score1 - score2;
}

parameters {
  real b;
  real c;
  real d;
  real e;
  real f;
  real<lower=0> sigma_a;
  real<lower=0> sigma_y;
  vector[nteams] eta_a;
}

transformed parameters {
  vector[nteams] a;
  a = b*prior_score + sigma_a*eta_a;
}

model {
  eta_a ~ normal(0, 1);
  for (i in 1:ngames)
    dif[i] ~ normal(a[team1[i]] - a[team2[i]] + 
                           c*ump1team1[i] + 
                           d*ump2team1[i] + 
                           e*hometeam1[i] + 
                           f*tossteam1[i], 
                           sigma_y);
}
