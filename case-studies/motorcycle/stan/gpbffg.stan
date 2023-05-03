functions {
#include gpbasisfun_functions.stan
}
data {
  int<lower=1> N;      // number of observations
  vector[N] x;         // univariate covariate
  vector[N] y;         // target variable
        
  real<lower=0> c_f;   // factor c to determine the boundary value L
  int<lower=1> M_f;    // number of basis functions
  real<lower=0> c_g;   // factor c to determine the boundary value L
  int<lower=1> M_g;    // number of basis functions

  real<lower=0> intercept_f_s;
  real<lower=0> intercept_g_s;
  real<lower=0> beta_f_s;
  real<lower=0> beta_g_s;
  real<lower=0> lscale_f_s;
  real<lower=0> lscale_g_s;
  real<lower=0> sigma_f_s;
  real<lower=0> sigma_g_s;

  real<lower=0> prior_alpha;
  real<lower=0> likelihood_alpha;
}
transformed data {
  // Normalize data
  real xmean = mean(x);
  real ymean = mean(y);
  real xsd = sd(x);
  real ysd = sd(y);
  vector[N] xn = (x - xmean)/xsd;
  vector[N] yn = (y - ymean)/ysd;
  // Basis functions for f
  real L_f = c_f*max(xn);
  matrix[N,M_f] PHI_f = PHI(N, M_f, L_f, xn);
  // Basis functions for g
  real L_g = c_g*max(xn);
  matrix[N,M_g] PHI_g = PHI(N, M_g, L_g, xn);
}
parameters {
  real intercept_f;
  real intercept_g;
  vector[M_f] beta_f;          // the basis functions coefficients
  vector[M_g] beta_g;          // the basis functions coefficients
  real<lower=0> lengthscale_f; // lengthscale of f
  real<lower=0> sigma_f;       // scale of f
  real<lower=0> lengthscale_g; // lengthscale of g
  real<lower=0> sigma_g;       // scale of g
}
model {
  // spectral densities for f and g
  vector[M_f] diagSPD_f = diagSPD_EQ(sigma_f, lengthscale_f, L_f, M_f);
  vector[M_g] diagSPD_g = diagSPD_EQ(sigma_g, lengthscale_g, L_g, M_g);
  // priors
  target += prior_alpha * normal_lpdf(intercept_f | 0, intercept_f_s);
  target += prior_alpha * normal_lpdf(intercept_g | 0, intercept_g_s);
  target += prior_alpha * normal_lpdf(beta_f | 0, beta_f_s);
  target += prior_alpha * normal_lpdf(beta_g | 0, beta_g_s);
  target += prior_alpha * normal_lpdf(lengthscale_f | 0, lscale_f_s);
  target += prior_alpha * normal_lpdf(lengthscale_g | 0, lscale_g_s);
  target += prior_alpha * normal_lpdf(sigma_f | 0, sigma_f_s);
  target += prior_alpha * normal_lpdf(sigma_g | 0, sigma_g_s);
  // model
  target += likelihood_alpha * normal_lpdf(yn | intercept_f + PHI_f * (diagSPD_f .* beta_f),
					   exp(intercept_g + PHI_g * (diagSPD_g .* beta_g)));
}
generated quantities {
  vector[N] f;
  vector[N] sigma;
  real log_prior;
  real log_lik;
  
  // spectral densities
  vector[M_f] diagSPD_f = diagSPD_EQ(sigma_f, lengthscale_f, L_f, M_f);
  vector[M_g] diagSPD_g = diagSPD_EQ(sigma_g, lengthscale_g, L_g, M_g);
  // function scaled back to the original scale
  f = (intercept_f + PHI_f * (diagSPD_f .* beta_f))*ysd + ymean;
  sigma = exp(intercept_g + PHI_g * (diagSPD_g .* beta_g))*ysd;

  log_prior = prior_alpha * (
			     normal_lpdf(intercept_f | 0, intercept_f_s) +
			     normal_lpdf(intercept_g | 0, intercept_g_s) +
			     normal_lpdf(beta_f | 0, beta_f_s) +
			     normal_lpdf(beta_g | 0, beta_g_s) +
			     normal_lpdf(lengthscale_f | 0, lscale_f_s) +
			     normal_lpdf(lengthscale_g | 0, lscale_g_s) +
			     normal_lpdf(sigma_f | 0, sigma_f_s) +
			     normal_lpdf(sigma_g | 0, sigma_g_s)
			     );
  
  log_lik = likelihood_alpha * normal_lpdf(yn | intercept_f + PHI_f * (diagSPD_f .* beta_f),
			exp(intercept_g + PHI_g * (diagSPD_g .* beta_g)));
}
