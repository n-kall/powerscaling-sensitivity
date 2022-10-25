library(tidyverse)
library(brms)
library(priorsense)
library(cmdstanr)
library(posterior)
library(purrr)
library(patchwork)
library(mvtnorm)
library(clusterGeneration)
library(future)

gen_x <- function(N, K) {

  #  sigma_X <- genPositiveDefMat(K)
  #  X <- rmvnorm(N, mean = rep(0, K), sigma = sigma_X$Sigma)
  X <- rmvnorm(N, mean = rep(0, K))

  colnames(X) <- paste0("X", 1:K)
  return(X)
}

gen_y <- function(X, sigma_Y) {
  b <- rep(1/ncol(X), times = ncol(X))
  mu <- X %*% b

  Y <- rt(nrow(X),, 3, mu, sigma_Y)
  return(Y)
}

sim_scale <- function(N, K, N_scaled, b_scales) {
  X <- gen_x(N, K)
  Y <- gen_y(X, sigma_Y = 1)

  map(b_scales, function(s) {
    X[,1:N_scaled] <- X[,1:N_scaled] / s
    X[,1:N_scaled] <- X[,1:N_scaled] / s

    cbind(X, Y)

  }
  )
}

scales <- c(0.125, 0.25, 0.5, 1, 2, 4, 8)

iters <- 1:100

dat <- map(iters, ~sim_scale(25, 4, 2, scales))

dat <- unlist(dat, recursive = FALSE)

fits_t <- brm_multiple(
  bf(Y ~ ., sigma = 1),
  data = dat,
  family = "gaussian",
  prior = c(prior(student_t(3, 0, 1), class = "b"),
            prior(normal(0, 10), class = "Intercept")), combine = FALSE, backend = "cmdstanr")

names(fits_t) <- paste0("i", iters, ",", rep(scales, times = length(iters)))

sens_t <- map_df(fits_t, ~powerscale_sensitivity(.)$sensitivity, .id = "iter-scale")

p_t <- sens_t |>
  separate("iter-scale", into = c("iter", "scale"), convert = TRUE, sep = ",") |>
  filter(variable %in% c("b_X1", "b_X2", "b_X3", "b_X4", "b_Intercept")) |>
  group_by(scale, variable) |>
  summarise(prior = mean(prior), likelihood = mean(likelihood)) |>
  gather(key = "component", value = "sensitivity", prior, likelihood) |>
  ggplot(aes(x = scale, y = sensitivity, color = variable, group = variable)) +
  facet_wrap(~component) +
  geom_line()


fits_n <- brm_multiple(
  bf(Y ~ ., sigma = 1),
  data = dat,
  family = "gaussian",
  prior = c(prior(normal(0, 1), class = "b"),
            prior(normal(0, 10), class = "Intercept"), combine = FALSE, backend = "cmdstanr")

  names(fits_n) <- paste0("i", iters, ",", rep(scales, times = length(iters)))

  sens_n <- map_df(fits_n, ~powerscale_sensitivity(.)$sensitivity, .id = "iter-scale")

  p_n <- sens_n |>
    separate("iter-scale", into = c("iter", "scale"), convert = TRUE, sep = ",") |>
    filter(variable %in% c("b_X1", "b_X2", "b_X3", "b_X4", "b_Intercept")) |>
    group_by(scale, variable) |>
    summarise(prior = mean(prior), likelihood = mean(likelihood)) |>
    gather(key = "component", value = "sensitivity", prior, likelihood) |>
    ggplot(aes(x = scale, y = sensitivity, color = variable, group = variable)) +
    facet_wrap(~component) +
    geom_line()

  p_t / p_n
