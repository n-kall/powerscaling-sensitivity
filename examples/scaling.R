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
library(cowplot)

set.seed(123)

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

  Y <- rnorm(nrow(X), mu, sigma_Y)
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

scales <- c(0.25, 0.5, 1, 2, 4)

iters <- 1:1000

dat <- map(iters, ~sim_scale(25, 4, 2, scales))

dat <- unlist(dat, recursive = FALSE)

## fits_t <- brm_multiple(
##   bf(Y ~ ., sigma = 1),
##   data = dat,
##   family = "gaussian",
##   prior = c(prior(student_t(3, 0, 1), class = "b"),
##             prior(normal(0, 10), class = "Intercept")), combine = FALSE, backend = "cmdstanr", seed = 123)

## names(fits_t) <- paste0("i", iters, ",", rep(scales, times = length(iters)))

## sens_t <- map_df(fits_t, ~powerscale_sensitivity(.)$sensitivity, .id = "iter-scale")

## saveRDS(sens_t, "sens_t.RDS")

## p_t <- sens_t |>
##   separate("iter-scale", into = c("iter", "scale"), convert = TRUE, sep = ",") |>
##   filter(variable %in% c("b_X1", "b_X2", "b_X3", "b_X4", "b_Intercept")) |>
##   group_by(scale, variable) |>
##   summarise(prior = mean(prior), likelihood = mean(likelihood)) |>
##   gather(key = "component", value = "sensitivity", prior, likelihood) |>
##   ggplot(aes(x = scale, y = sensitivity, color = variable, group = variable)) +
##   facet_wrap(~component) +
##   geom_line()


fits_n <- brm_multiple(
  bf(Y ~ ., sigma = 1),
  data = dat,
  family = "gaussian",
  prior = c(prior(normal(0, 1), class = "b"),
            prior(normal(0, 10), class = "Intercept")), combine = FALSE, backend = "cmdstanr", seed = 123)

  names(fits_n) <- paste0("i", iters, ",", rep(scales, times = length(iters)))

  sens_n <- map_df(fits_n, ~powerscale_sensitivity(.)$sensitivity, .id = "iter-scale")

saveRDS(sens_n, "sens_n1000.RDS")

sens_n <- readRDS("sens_n.RDS")

#sens_t <- readRDS("sens_t.RDS")

  p_n <- sens_n |>
    separate("iter-scale", into = c("iter", "scale"), convert = TRUE, sep = ",") |>
    filter(variable %in% c("b_X1", "b_X2", "b_X3", "b_X4"),
           scale > 0.125, scale < 8) |>
    gather(key = "component", value = "sensitivity", prior, likelihood) |>
    group_by(scale, variable, component) |>
    summarise(mean = mean(sensitivity), sd = sd(sensitivity)) |>
    mutate(unscaled = ifelse(variable %in% c("b_X1", "b_X2"), FALSE, TRUE)) |>
    ggplot(aes(x = scale, y = mean, color = unscaled, shape = unscaled)) +
    facet_wrap(~component, labeller = ggplot2::labeller(
      component = c(
        likelihood = "Likelihood power-scaling",
        prior = "Prior power-scaling"
        ))) +
    geom_point() +
    scale_color_brewer(type = "qual", labels = c("Scaled: $\\beta_1, \\beta_2$", "Unscaled: $\\beta_3, \\beta_4$")) +
  #  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.05)+
  #  geom_line() +
    scale_x_log10(breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                  labels = c("0.125", "0.25", "0.5", "1", "2", "4", "8")) +
    scale_shape_manual(values = c(17, 19), labels = c("Scaled: $\\beta_1, \\beta_2$", "Unscaled: $\\beta_3, \\beta_4$")) +
    ylab("Sensitivity ($D_{\\text{CJS}}$)") +
    xlab("Covariate scaling factor ($c$)") +
    cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = c(0.02, 0.2),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
    ) +
    geom_line(aes(group = variable)) + 
    cowplot::panel_border("black", size = 0.5)

save_tikz_plot(p_n, "../figs/covariate_scaling_example.tex", width = 5.2, height = 2.5)
