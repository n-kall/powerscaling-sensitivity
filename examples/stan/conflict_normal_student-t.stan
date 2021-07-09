data {
  real df_prior;
  real location_prior;
  real scale_prior;
  real df_lik;
  real location_lik;
  real scale_lik;
  real prior_alpha;
  real likelihood_alpha;
}

parameters {
  real mu_post;
  real mu_prior;
  real mu_lik;
}
transformed parameters {
  real log_prior;
  real log_lik;
  log_prior = student_t_lpdf(mu_post | df_prior, location_prior, scale_prior);
  log_lik = student_t_lpdf(mu_post | df_lik, location_lik, scale_lik);
}
model {
  target += prior_alpha * student_t_lpdf(mu_prior | df_prior, location_prior, scale_prior);
  target += likelihood_alpha * student_t_lpdf(mu_lik | df_lik, location_lik, scale_lik);
  target += prior_alpha * log_prior;
  target += likelihood_alpha * log_lik;
}

