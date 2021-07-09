library(targets)

# custom functions
source("R/functions.R")
options(mc.cores = parallel::detectCores())
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
  packages = c("tidyverse", "brms", "priorsense", "rstan", "cowplot", "tikzDevice", "patchwork"),
  imports = "priorsense"
)

# End this file with a list of target objects.
list(
  tar_target(
    name = data,
    command = prepare_data()
  ),
  tar_target(
    name = stancode_file,
    command = "stan/bacteria_stan.stan",
    format = "file"
  ),
  tar_target(
    compiled_model,
    stan_model(stancode_file)
  ),
  tar_target(
    fit_prior0,
    create_fit(compiled_model, data, 0 )
  ),
  tar_target(
    fit_prior1,
    create_fit(compiled_model, data, 1)
  ),
  tar_target(
    fit_prior2,
    create_fit(compiled_model, data, 2)
  ),
  tar_target(
    fit_prior4,
    create_fit(compiled_model, data, 4)
  ),
  tar_target(
    fit_prior5,
    create_fit(compiled_model, data, 5)
  ),
  tar_target(
    sensitivity_prior0,
    sensitivity_analysis(fit_prior0)
  ),
  tar_target(
    sensitivity_prior1,
    sensitivity_analysis(fit_prior1)
  ),
  tar_target(
    sensitivity_prior2,
    sensitivity_analysis(fit_prior2)
  ),
  tar_target(
    sensitivity_prior4,
    sensitivity_analysis(fit_prior4)
  ),
  tar_target(
    sensitivity_prior5,
    sensitivity_analysis(fit_prior5)
  ),
  tar_target(
    prior_plot,
    plot_priors()
  ),
  tar_target(
    sensitivity_seq1,
    sensitivity_sequence(compiled_model, data, 1)
  ),
  tar_target(
    sensitivity_seq2,
    sensitivity_sequence(compiled_model, data, 2)
  ),
  tar_target(
    prior_plot_tikz,
    save_tikz_plot(prior_plot, "../../figs/bacteria_priors.tex", width = 5, height = 2)
  )
)


