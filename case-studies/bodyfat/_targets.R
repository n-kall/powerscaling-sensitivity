library(targets)

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


# custom functions
source("R/functions.R")

# Set target-specific options such as packages.
tar_option_set(
  packages = c(
    "tidyverse",
    "rstan",
    "loo",
    "posterior",
    "brms",
    "lme4",
    "viridis",
    "bayesplot",
    "patchwork",
    "priorsense",
    "tikzDevice"
  ),
  imports = "priorsense"
)

# End this file with a list of target objects.
list(
  tar_target(
    name = data_file,
    command = "data/bodyfat.txt",
    format = "file"
  ),
  tar_target(
    name = stancode_file,
    command = "stan/bodyfat.stan",
    format = "file"
  ),
  tar_target(
    name = formula,
    command = create_formula()
  ),
  tar_target(
    name = cleandata,
    command = prepare_data(data_file)
  ),
  tar_target(
    name = standata,
    command = prepare_standata(formula, cleandata)
  ),
  tar_target(
    name = compiled_model,
    command = stan_model(stancode_file)
  ),
  tar_target(
    name = fit_base,
    command = run_model(compiled_model, standata, c(auto_prior = 0, prior_width = 1))
  ),
  tar_target(
    name = brm_base,
    command = create_brm_model(formula, cleandata, fit_base, c(auto_prior = 0, prior_width = 1))
  ),
  tar_target(
    name = sensitivity_base,
    command = sensitivity_analysis(brm_base)
  ),
  tar_target(
    name = sensitivity_sequence_base,
    powerscale_seq_mm(auto_prior = 0)
  ),
  tar_target(
    name = sensitivity_plot_base,
    command = powerscale_seq_plot(sensitivity_sequence_base)
  ),
  tar_target(
    name = quantities_plot_base,
    command = powerscale_seq_summ_plot(sensitivity_sequence_base)
  ),
  tar_target(
    name = fit_autoprior,
    command = run_model(compiled_model, standata, c(auto_prior = 1, prior_width = 1))
  ),
  tar_target(
    name = brm_autoprior,
    command = create_brm_model(formula, cleandata, fit_autoprior, c(auto_prior = 1, prior_width = 1))
  ),
  tar_target(
    name = sensitivity_autoprior,
    command = sensitivity_analysis(brm_autoprior)
  ),
  tar_target(
    name = sensitivity_sequence_autoprior,
    command = powerscale_seq_mm(auto_prior = 1)
  ),
  tar_target(
    name = sensitivity_plot_autoprior,
    command = powerscale_seq_plot(sensitivity_sequence_autoprior)
  ),
  tar_target(
    sensitivity_plot_base_tikz,
    save_tikz_plot(sensitivity_plot_base + guides(linetype = FALSE), "../../figs/bodyfat_sens_base.tex", 5, 2)
  ),
  tar_target(
    quantities_plot_base_tikz,
    save_tikz_plot(quantities_plot_base, "../../figs/bodyfat_quantities_base.tex", 5, 3.5)
  )  
)
