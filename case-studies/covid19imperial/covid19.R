library(cmdstanr)
library(posteriordb)
library(posterior)

devtools::load_all("~/repo/priorsense")

po <- posterior("ecdc0401-covid19imperial_v3")
dat <- pdb_data(po)

m <- cmdstan_model("stan/covid19imperial_v3.stan")

f <- m$sample(data = dat, parallel_chains = 4, chains = 4)

# tau prior
p1 <- powerscale_sensitivity(f, variable = "prediction", prior_selection = 1)

# phi prior
p2 <- powerscale_sensitivity(f, variable = "prediction", prior_selection = 2)

# kappa prior
p3 <- powerscale_sensitivity(f, variable = "prediction", prior_selection = 3)


arrange(p1$sensitivity, desc(prior))

arrange(p2$sensitivity, desc(prior))

arrange(p3$sensitivity, desc(prior))
