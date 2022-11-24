library(brms)
library(priorsense)
library(ggplot2)
library(ggdist)
library(dplyr)
library(posterior)

load_data <- function() {

  data(UScrime, package = "MASS")
  UScrime[, -c(2, ncol(UScrime))] <- log(UScrime[, -c(2, ncol(UScrime))])

  UScrime
}

make_formula <- function(family) {
bf(y ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + GDP + Ineq + Prob + Time, family = family)
}

dat <- load_data()

f_orig <- brm(make_formula("lognormal"), prior = prior(R2D2(mean_R2 = 0.3, prec_R2 = 10)), data = dat)


psens_orig <- powerscale_sensitivity(f_orig, prediction = function(x) draws_df(R2 = bayes_R2(x, summary = F), .nchains = 4))

filter(psens_orig$sensitivity, variable == "R2")

ps_orig <- powerscale_sequence(f_orig, prediction = function(x) draws_df(R2 = bayes_R2(x, summary = F), .nchains = 4))

p1 <- powerscale_plot_quantities(ps_orig, variables = c("R2"), quantities = "mean") + ggplot2::ggtitle("mean_R2 = 0.3, prec_R2 = 10", subtitle = NULL)

f_alt <- brm(make_formula("lognormal"), prior = prior(R2D2(mean_R2 = 0.3, prec_R2 = 1.5)), data = load_data(), control = list(adapt_delta = 0.99))

autop <- sjstats::auto_prior(make_formula("lognormal"), data = load_data(), gaussian = FALSE)

f_unif <- brm(make_formula("lognormal"), data = load_data(), control = list(adapt_delta = 0.99))

f_auto <- brm(make_formula("lognormal"), data = load_data(), prior = autop, control = list(adapt_delta = 0.99))

psens_alt <- powerscale_sensitivity(f_alt, prediction = function(x) draws_df(R2 = bayes_R2(x, summary = FALSE), .nchains = 4))

filter(psens_alt$sensitivity, variable == "R2")

ps_alt <- powerscale_sequence(f_alt,  prediction = function(x) draws_df(R2 = bayes_R2(x, summary = FALSE), .nchains = 4))

p2 <- powerscale_plot_quantities(ps_alt, variables = c("R2"), quantities = "mean") + ggplot2::ggtitle("mean_R2 = 0.3, prec_R2 = 1.5", subtitle = NULL)


pars_orig <- simstudy::betaGetShapes(0.3, 10)

pars_alt <- simstudy::betaGetShapes(0.3, 1.5)

ggplot(data = tibble(x = c(0, 1)), aes(x)) +
  geom_function(n = 10000, fun = dbeta, args = list(shape1 = pars_orig[[1]], shape2 = pars_orig[[2]]), colour = "blue") +
  geom_function(n = 10000, fun = dbeta, args = list(shape1 = pars_alt[[1]], shape2 = pars_alt[[2]]), colour = "red") +
  xlim(0.001, 0.999)
