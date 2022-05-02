library(targets)
library(stantargets)

# custom functions
source("R/functions.R")
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

# Set target-specific options such as packages.
tar_option_set(
  packages = c("tidyverse", "priorsense", "cowplot", "tikzDevice", "patchwork", "cmdstanr"),
  imports = "priorsense"
)

# End this file with a list of target objects.
list(
  tar_stan_mcmc(
    name = prior0,
    stan_file = "stan/bacteria_stan.stan",
    data = c(prepare_data(), prior = 0),
    iter_warmup = 2000,
    iter_sampling = 8000,
    seed = 1234
  ),
  tar_stan_mcmc(
    name = prior1,
    stan_files = "stan/bacteria_stan.stan",
    data = c(prepare_data(), prior = 1),
    iter_warmup = 2000,
    iter_sampling = 8000,
    seed = 1234
  ),
  tar_stan_mcmc(
    name = prior2,
    stan_files = "stan/bacteria_stan.stan",
    data = c(prepare_data(), prior = 2),
    iter_warmup = 2000,
    iter_sampling = 8000,
    seed = 1234,
    chains = 4,
    parallel_chains = 4
  ),
  tar_stan_mcmc(
    name = prior4,
    stan_files = "stan/bacteria_stan.stan",
    data = c(prepare_data(), prior = 4),
    iter_warmup = 2000,
    iter_sampling = 8000,
    seed = 1234
  ),
  tar_stan_mcmc(
    name = prior5,
    stan_files = "stan/bacteria_stan.stan",
    data = c(prepare_data(), prior = 5),
    iter_warmup = 2000,
    iter_sampling = 8000,
    seed = 1234
  ),
  tar_target(
    sensitivity_prior0,
    sensitivity_analysis(prior0_mcmc_bacteria_stan)
  ),
  tar_target(
    sensitivity_prior1,
    sensitivity_analysis(prior1_mcmc_bacteria_stan)
  ),
  tar_target(
    sensitivity_prior2,
    sensitivity_analysis(prior2_mcmc_bacteria_stan)
  ),
  tar_target(
    sensitivity_prior4,
    sensitivity_analysis(prior4_mcmc_bacteria_stan)
  ),
  tar_target(
    sensitivity_prior5,
    sensitivity_analysis(prior5_mcmc_bacteria_stan)
  ),
  tar_target(
    prior_plot,
    plot_priors()
  ),
  tar_target(
    prior_plot_tikz,
    save_tikz_plot(prior_plot, "../../figs/bacteria_priors.tex", width = 5.2, height = 2)
  )
)


