data {

  int<lower=1> N; // total number of observations
  array[N] int Y; // response variable
  int<lower=1> K; // number of population-level effects
  matrix[N, K] X; // population-level design matrix
  array[K] real sigma; // sigma for priors
}
parameters {
  vector[K] b; // population-level effects
}
transformed parameters {
  real lprior = 0; // prior contributions to the log posterior
  lprior += student_t_lpdf(b | 4, 0, sigma);
}
model {
  // likelihood including constants
    target += bernoulli_logit_glm_lpmf(Y | X, 0, b);
  // priors including constants
  target += lprior;
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(Y[n] | X[n] * b);
  }
}
