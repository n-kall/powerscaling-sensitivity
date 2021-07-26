library(cmdstanr)
library(priorsense)
library(posterior)
library(tikzDevice)
library(tidyverse)
library(cowplot)
library(patchwork)
library(metRology)
library(stringi)

options(
  tizkDocumentDelcaration = "\\documentclass[10pt]{article}",
  tikzLatexPackages = c(
    "\\usepackage{tikz}",
    "\\usepackage{amsmath}",
    "\\usepackage{amsfonts}",
    "\\usepackage{bm}",
    "\\usepackage{lmodern}",
    "\\usepackage{multirow}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage{textcomp}",
    "\\usepackage{microtype}",
    "\\DeclareMathOperator{\\normal}{normal}",
    "\\DeclareMathOperator{\\Bernoulli}{Bernoulli}",
    "\\DeclareMathOperator{\\gammadist}{gamma}",
    "\\DeclareMathOperator{\\expdist}{exponential}",
    "\\DeclareMathOperator{\\betadist}{beta}",
    "\\DeclareMathOperator{\\Cauchy}{Cauchy}"
  )
)

iter_sampling <- 100000
iter_warmup <- 10000

mcmc_scaling <- function(model, iter_sampling, iter_warmup,
                         data, parameter = "mu") {

  par_post <- str_c(parameter, "post", sep = "_")
  par_lik <- str_c(parameter, "lik", sep = "_")
  par_prior <- str_c(parameter, "prior", sep = "_")

  fit_base <- model$sample(
    data = c(data, prior_alpha = 1, likelihood_alpha = 1),
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    )
  
  fit_weak_likelihood <- model$sample(
    data = c(data, list(likelihood_alpha = 0.5, prior_alpha = 1)),
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    )

  fit_weak_prior <- model$sample(
    data = c(data, list(likelihood_alpha = 1, prior_alpha = 0.5)),
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    )

  fit_strong_likelihood <- model$sample(
    data = c(data, list(likelihood_alpha = 2, prior_alpha = 1)),
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    )

  fit_strong_prior <- model$sample(
    data = c(data, list(prior_alpha = 2, likelihood_alpha = 1)),
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    )

  draws_base <- as_draws_df(fit_base$draws(variables = c(par_post, par_prior, par_lik))) %>%
    gather(c("mu_post", "mu_prior", "mu_lik"), key = variable, value = value) %>%
    mutate(scaled_component = "Prior power-scaling", scaling = "base")

  draws_base2 <- as_draws_df(fit_base$draws(variables = c(par_post, par_prior, par_lik))) %>%
    gather(c(par_post, par_prior, par_lik), key = variable, value = value) %>%
    mutate(scaled_component = "Likelihood power-scaling", scaling = "base")

  draws_weak_likelihood <- as_draws_df(
    fit_weak_likelihood$draws(
      variables = c(par_post, par_prior, par_lik)
    )
  ) %>%
    gather(c(par_post, par_prior, par_lik), key = variable, value = value) %>%
    mutate(scaled_component = "Likelihood power-scaling", scaling = "weak")

  draws_strong_likelihood <- as_draws_df(
    fit_strong_likelihood$draws(
      variables = c(par_post, par_prior, par_lik)
    )
  ) %>%
    gather(c(par_post, par_prior, par_lik), key = variable, value = value) %>%
    mutate(scaled_component = "Likelihood power-scaling", scaling = "strong")

  draws_weak_prior <- as_draws_df(
    fit_weak_prior$draws(
      variables = c(par_post, par_prior, par_lik)
    )
  ) %>%
    gather(c(par_post, par_prior, par_lik), key = variable, value = value) %>%
    mutate(scaled_component = "Prior power-scaling", scaling = "weak")

  draws_strong_prior <- as_draws_df(
    fit_strong_prior$draws(
      variables = c(par_post, par_prior, par_lik)
    )
  ) %>%
    gather(c(par_post, par_prior, par_lik), key = variable, value = value) %>%
    mutate(scaled_component = "Prior power-scaling", scaling = "strong")

  combined_draws <- bind_rows(
    draws_base,
    draws_base2,
    draws_weak_prior,
    draws_strong_prior,
    draws_weak_likelihood,
    draws_strong_likelihood
  ) %>%
    mutate(
      scaling = factor(scaling, levels = c("strong", "base", "weak"),
                       labels = c("$\\alpha = 2$", "$\\alpha = 1$", "$\\alpha = 0.5$")),
      variable = fct_recode(variable, prior = par_prior, posterior = par_post, likelihood = par_lik),
      variable = fct_relevel(variable, "prior", "likelihood", "posterior"),
      scaled_component = factor(scaled_component, levels = c("Prior power-scaling", "Likelihood power-scaling")),
      )%>%
    rename(distribution = variable)

  return(combined_draws)
  
}



mcmc_scaling_plot <- function(combined_draws, y_positions, prior_sd, prior_df, likelihood_sd = 1, likelihood_df, component = c("prior", "likelihood")) {

  combined_draws <- combined_draws %>%
    filter(scaled_component %in% 
             paste(stri_trans_totitle(component), "power-scaling")
           )

  analytical_prior <- tibble(
    x = seq(-5, 15, 0.1),
    "$\\alpha = 1$" = dt.scaled(x, df = prior_df, mean = 0, sd = prior_sd),
    "$\\alpha = 0.5$" = dt.scaled(x, df = prior_df, mean = 0, sd = prior_sd*sqrt(2)),
    "$\\alpha = 2$" = dt.scaled(x, df = prior_df, mean = 0, sd = prior_sd/sqrt(2)),
    distribution = "prior",
    scaled_component = "Prior power-scaling"
  )

  analytical_likelihood <- tibble(
    x = seq(-5, 15, 0.1),
    "$\\alpha = 1$" = dt.scaled(x, df = likelihood_df, mean = 10, sd = likelihood_sd),
    "$\\alpha = 0.5$" = dt.scaled(x, df = likelihood_df, mean = 10, sd = likelihood_sd*sqrt(2)),
    "$\\alpha = 2$" = dt.scaled(x, df = likelihood_df, mean = 10, sd = likelihood_sd/sqrt(2)),
    distribution = "likelihood",
    scaled_component = "Likelihood power-scaling"
  )

  analytical_prior_noscale <- analytical_prior %>%
    mutate(
      "$\\alpha = 0.5$" = `$\\alpha = 1$`,
      "$\\alpha = 2$" = `$\\alpha = 1$`,
      scaled_component = "Likelihood power-scaling"
    )

  analytical_likelihood_noscale <- analytical_likelihood %>%
    mutate(
      "$\\alpha = 0.5$" = `$\\alpha = 1$`,
      "$\\alpha = 2$" = `$\\alpha = 1$`,
      scaled_component = "Prior power-scaling"
    )
  


  if (!("prior" %in% component)) {

    analytical_prior <- tibble()
    analytical_likelihood_noscale <- tibble()

  }

  if (!("likelihood" %in% component)) {

    analytical_prior_noscale <- tibble()
    analytical_likelihood <- tibble()

  }
  

  analytical <- analytical_prior %>%
    bind_rows(analytical_prior_noscale, analytical_likelihood, analytical_likelihood_noscale) %>%
    gather(key = "scaling", value = "density", -scaled_component, -x, -distribution) %>%
    mutate(
      distribution = factor(distribution, levels = c("prior", "likelihood", "posterior")),
      scaled_component = factor(scaled_component, levels = c("Prior power-scaling", "Likelihood power-scaling"))
    )
  
  alpha_labels <- tibble(
    scaling = factor(rep(c("$\\alpha = 2$", "$\\alpha = 1$", "$\\alpha = 0.5$"), 2)),
    scaled_component = factor(rep(c("Prior power-scaling", "Likelihood power-scaling"), each = 3), levels = c("Prior power-scaling", "Likelihood power-scaling")),
    distribution = factor(rep(c("prior", "likelihood"), each = 3), levels = c("prior", "likelihood", "posterior")),
    x = rep(c(0, 10), each = 3),
    y = rep(y_positions)
  ) %>%
    filter(scaled_component %in% 
             paste(stri_trans_totitle(component), "power-scaling")
           )

  
  p <- combined_draws %>%
    mutate(distribution = factor(distribution, levels = c("prior", "likelihood", "posterior"))) %>%
    filter(distribution == "posterior") %>% 
    ggplot(aes(x = value, color = distribution)) +
    # analytical density
    geom_line(aes(x = x, y = density, color = distribution), data = analytical, size = 1.1) +
    # shaded posterior density
    geom_density(aes(color = distribution, fill = distribution), alpha = 0.25, size = 1.1, trim = FALSE, show.legend = FALSE) +
    scale_color_brewer(type = "qual", palette = "Set2", drop = FALSE) +
    scale_fill_brewer(type = "qual", palette = "Set2", drop = FALSE) +
    facet_grid(scaled_component ~ scaling, drop = TRUE, switch = "y") +
    geom_text(aes(x = x, y = y, label = scaling), data = alpha_labels, show.legend = FALSE) +
    theme_cowplot() +
    theme(strip.background.y = element_blank(), strip.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +
    xlab("$\\theta$") +
    panel_border() +
    ylab("") +
    xlim(-5, 15)

  

  return(p)

}

normal_prior_normal_lik <- function(model, iter_sampling, iter_warmup) {
  mcmc_scaling(
    model = model,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    data = list(
      df_prior = 1000,
      location_prior = 0,
      scale_prior = 2.5,
      df_lik = 1000,
      location_lik = 10,
      scale_lik = 1
    )
  )
}

scaling_example_plot <- function(draws) {
  mcmc_scaling_plot(draws, c(0.3, 0.25, 0.2, 0.65, 0.5, 0.35), prior_sd = 2.5, prior_df = 1000, likelihood_df = 1000, component = "prior") +
    theme(
      strip.text = element_blank(),
      legend.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray"),
      strip.background = element_blank(),
      legend.position = c(0.01, 0.9),
      legend.title = element_blank()
    )  +
    guides(color = guide_legend(keywidth = 0.2, keyheight = 0.15, default.unit = "inch"))
}

normal_prior_normal_lik_plot <- function(draws) {
  mcmc_scaling_plot(draws, c(0.3, 0.25, 0.2, 0.65, 0.5, 0.4), prior_sd = 2.5, prior_df = 1000, likelihood_df = 1000, component = c("prior", "likelihood")) +
    theme(
      legend.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      strip.text = element_text(size = rel(0.6)),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray"),
      strip.background = element_blank(),
      legend.position = c(0.01, 0.95),
      legend.title = element_blank()
    ) +
    guides(color = guide_legend(keywidth = 0.2, keyheight = 0.15, default.unit = "inch"))
}

normal_prior_t_lik <- function(model, iter_sampling, iter_warmup) {
  mcmc_scaling(
    model = model,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    data = list(
      df_prior = 1000,
      location_prior = 0,
      scale_prior = 1,
      df_lik = 4,
      location_lik = 10,
      scale_lik = 1
    )
  )
}

normal_prior_t_lik_plot <- function(draws) {
  mcmc_scaling_plot(draws, rep(c(0.65, 0.5, 0.35), 2), prior_sd = 1, prior_df = 1000, likelihood_df = 4, component = c("prior", "likelihood")) +
    theme(
      legend.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      strip.text = element_text(size = rel(0.6)),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray"),
      strip.background = element_blank(),
      legend.position = c(0.01, 0.95),
      legend.title = element_blank(),
      ) +
    guides(color = guide_legend(keywidth = 0.2, keyheight = 0.15, default.unit = "inch"))
}
## # weakly informative prior

weakly_inf_normal_prior_normal_lik <- function(model, iter_sampling, iter_warmup) {
  mcmc_scaling(
    model = model,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    data = list(
      df_prior = 1000,
      location_prior = 0,
      scale_prior = 10,
      df_lik = 1000,
      location_lik = 10,
      scale_lik = 1
    )
  )
}

weakly_inf_normal_prior_normal_lik_plot <- function(draws) {
  mcmc_scaling_plot(draws, c(0.2, 0.2, 0.2, 0.6, 0.5, 0.4), prior_sd = 10, prior_d = 1000, likelihood_df = 1000) +
    theme(
      legend.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      strip.text = element_text(size = rel(0.6)),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray"),
      strip.background = element_blank(),
      legend.position = c(0.01, 0.95),
      legend.title = element_blank()
    )  +
    guides(color = guide_legend(keywidth = 0.2, keyheight = 0.15, default.unit = "inch"))
}
