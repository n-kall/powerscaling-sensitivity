// generated with brms 2.14.4
//test
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
  int auto_prior;
  real<lower=0> prior_width;
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
  real<lower=0> sigma;  // residual SD
}
transformed parameters {
  real log_prior = 0;
  if (auto_prior == 0) {
    // priors including all constants
    log_prior += normal_lpdf(b | 0, prior_width);
  } else {
    // priors including all constants
    log_prior += normal_lpdf(b[1] | 0, 1.63);
    log_prior += normal_lpdf(b[2] | 0, 0.77);
    log_prior += normal_lpdf(b[3] | 0, 7.89);
    log_prior += normal_lpdf(b[4] | 0, 9.06);
    log_prior += normal_lpdf(b[5] | 0, 2.56);
    log_prior += normal_lpdf(b[6] | 0, 2.04);
    log_prior += normal_lpdf(b[7] | 0, 3.2);
    log_prior += normal_lpdf(b[8] | 0, 4.21);
    log_prior += normal_lpdf(b[9] | 0, 8.95);
    log_prior += normal_lpdf(b[10] | 0, 12.57);
    log_prior += normal_lpdf(b[11] | 0, 7.09);
    log_prior += normal_lpdf(b[12] | 0, 10.25);
    log_prior += normal_lpdf(b[13] | 0, 22.7);
  }
  log_prior += student_t_lpdf(Intercept | 3, 19.2, 9.2);
  log_prior += student_t_lpdf(sigma | 3, 0, 9.2)
    - 1 * student_t_lccdf(0 | 3, 0, 9.2);

}
model {
  // likelihood including all constants
  if (!prior_only) {
    target += normal_id_glm_lpdf(Y | Xc, Intercept, b, sigma);
  }
  target += log_prior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  vector[N] log_lik;
  for (n in 1:N) log_lik[n] = normal_lpdf(Y[n] | Xc[n, ] * b + Intercept, sigma);
}
