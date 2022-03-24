data {
  int<lower=1> N;  // number of observations
  int y[N];  // response variable
  int week[N]; // weeks
  int trtDrug[N]; // treatment drug
  int trtDrugP[N]; // treatment drug+
  int<lower=1> K; // number of groups
  int<lower=1> ID[N]; // group indicator
  int<lower=0> prior;
}

parameters {
  vector[K] b_Intercept;
  real<lower=0> sigma;
  real mu;
  real b_week;
  real b_trtDrug;
  real b_trtDrugP;
}

transformed parameters {
  real theta[N];
  real log_prior;

  real<lower=0> tau = 1/(square(sigma));
  if (prior == 0) {
    // baseline
    log_prior = gamma_lpdf(tau | 0.01, 0.01);
  } else if (prior == 1) {
    // alt 1. half normal
    log_prior = normal_lpdf(tau | 0, 10);
  } else if (prior == 2) {
   // alt 2. half cauchy
    log_prior = cauchy_lpdf(tau | 0, 100);
  } else if (prior == 3) {
    // alt 3. uniform
  } else if (prior == 4) {
    // alt 4. gamma(1, 2)
    log_prior = gamma_lpdf(tau | 1, 2);
  } else if (prior == 5) {
    log_prior = gamma_lpdf(tau | 9, 0.5);
  }
    
  log_prior += normal_lpdf(mu | 0, 10);
  log_prior += normal_lpdf(b_week | 0, 10);
  log_prior += normal_lpdf(b_trtDrug | 0, 10);
  log_prior += normal_lpdf(b_trtDrugP | 0, 10);

  for (n in 1:N) {
    theta[n] =
      mu // population intercept
      + b_Intercept[ID[n]]
      + b_week * week[n]
      + b_trtDrug * trtDrug[n]
      + b_trtDrugP * trtDrugP[n];
  }
}
model {
  // prior
  target += log_prior;

  // hierarchical component
  target += normal_lpdf(b_Intercept | 0, sigma);
  
  // likelihood
  target += bernoulli_logit_lpmf(y | theta);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) log_lik[n] = bernoulli_logit_lpmf(y[n] | theta[n]);
}
