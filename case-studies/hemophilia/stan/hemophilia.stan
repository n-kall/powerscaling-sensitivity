// generated with brms 2.14.4
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
}
transformed parameters {
  real log_prior;
  log_prior = normal_lpdf(b[1] | 0, 10)
   + normal_lpdf(b[2] | -10, 5)
   + normal_lpdf(Intercept | 0, 10);
}
model {
  // likelihood including all constants
  if (!prior_only) {
    target += bernoulli_logit_glm_lpmf(Y | Xc, Intercept, b);
  }
  // priors including all constants
  target += log_prior;
}
generated quantities {
  // actual population-level intercept
  vector[N] log_lik;
  real b_Intercept = Intercept - dot_product(means_X, b);

  for (n in 1:N)
    log_lik[n] = bernoulli_logit_lpmf(Y[n] | Xc[n, ] * b + Intercept);
  
}
