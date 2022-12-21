library(cmdstanr)

m_normal <- cmdstan_model("bernoulli_logit.stan", compile = TRUE)

m_student <- cmdstan_model("bernoulli_logit_t.stan", compile = TRUE)
