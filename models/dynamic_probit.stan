data {
  int<lower=1> N;
  int<lower=1, upper=N> N_train;
  int<lower=2> J;
  int<lower=2> Q;
  int<lower=1> C;
  int<lower=1> G;
  array[N] int<lower=1, upper=J> a;
  array[N] int<lower=1, upper=J> b;
  array[N] int<lower=1, upper=Q> quarter;
  array[N] int<lower=1, upper=C> country;
  array[N] int<lower=1, upper=G> ground;
  vector[N] home;
  vector[N] era;
  array[N_train] int<lower=1, upper=3> y;
  real draw_prior;
  int<lower=0, upper=1> prior_only;
}
parameters {
  matrix[J, Q] z_skill;
  real<lower=0> sigma_walk;
  real home_mean;
  vector[C] z_country_home;
  vector[G] z_ground_home;
  real<lower=0> sigma_country_home;
  real<lower=0> sigma_ground_home;
  real log_draw;
  real z_draw_trend;
  vector[C] z_country_draw;
  vector[G] z_ground_draw;
  real<lower=0> sigma_country_draw;
  real<lower=0> sigma_ground_draw;
}
transformed parameters {
  real draw_trend = 0.05 * z_draw_trend;
  matrix[J, Q] skill;
  vector[C] country_home = sigma_country_home * z_country_home;
  vector[G] ground_home = sigma_ground_home * z_ground_home;
  vector[C] country_draw = sigma_country_draw * z_country_draw;
  vector[G] ground_draw = sigma_ground_draw * z_ground_draw;
  vector[N] eta;
  vector<lower=0>[N] margin;
  skill[, 1] = z_skill[, 1] - mean(z_skill[, 1]);
  for (q in 2:Q) {
    skill[, q] = skill[, q - 1] + sigma_walk * (z_skill[, q] - mean(z_skill[, q]));
  }
  for (n in 1:N) {
    eta[n] = skill[a[n], quarter[n]] - skill[b[n], quarter[n]]
             + home[n] * (home_mean + country_home[country[n]] + ground_home[ground[n]]);
    margin[n] = exp(log_draw + draw_trend * era[n]
                    + country_draw[country[n]] + ground_draw[ground[n]]);
  }
}
model {
  to_vector(z_skill) ~ std_normal();
  sigma_walk ~ normal(0, 0.3);
  home_mean ~ normal(0, 0.75);
  z_country_home ~ std_normal();
  z_ground_home ~ std_normal();
  sigma_country_home ~ normal(0, 0.3);
  sigma_ground_home ~ normal(0, 0.3);
  log_draw ~ normal(draw_prior, 1);
  z_draw_trend ~ std_normal();
  z_country_draw ~ std_normal();
  z_ground_draw ~ std_normal();
  sigma_country_draw ~ normal(0, 0.5);
  sigma_ground_draw ~ normal(0, 0.5);
  if (!prior_only) {
    for (n in 1:N_train) {
      y[n] ~ ordered_probit(eta[n], [-margin[n], margin[n]]');
    }
  }
}
generated quantities {
  matrix[N - N_train, 3] p;
  vector[3] replicated_counts = rep_vector(0, 3);
  for (n in 1:N_train) {
    int replicated = ordered_probit_rng(eta[n], [-margin[n], margin[n]]');
    replicated_counts[replicated] += 1;
  }
  if (N > N_train) {
    for (n in (N_train + 1):N) {
      for (k in 1:3) {
        p[n - N_train, k] = exp(ordered_probit_lpmf(k | eta[n], [-margin[n], margin[n]]'));
      }
    }
  }
}
