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
    "dplyr",
    "ggplot2",
    "stringr",
    "readr",
    "tibble",
    "rstan",
    "tidyverse",
    "loo",
    "posterior",
    "brms",
    "tidybayes",
    "cowplot",
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
    name = formula,
    command = create_formula()
  ),
  tar_target(
    name = cleandata,
    command = prepare_data(data_file)
  ),
  tar_target(
    name = brm_base,
    command = create_brm_model(formula, cleandata, prior = "base")
  ),
  tar_target(
    posterior_plot_base,
    posterior_plot(brm_base)
  ),
  tar_target(
    name = sensitivity_base,
    command = sensitivity_analysis(brm_base)
  ),
  tar_target(
    name = sensitivity_sequence_base,
    sensitivity_sequence_mm(data = cleandata, formula = formula, prior = "base")
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
    name = brm_auto,
    command = create_brm_model(formula, cleandata, prior = "auto")
  ),
  tar_target(
    name = sensitivity_auto,
    command = sensitivity_analysis(brm_auto)
  ),
  tar_target(
    name = sensitivity_sequence_auto,
    sensitivity_sequence_mm(data = cleandata, formula = formula, prior = "auto")
  ),
  tar_target(
    name = sensitivity_plot_auto,
    command = powerscale_seq_plot(sensitivity_sequence_auto)
  ),
  tar_target(
    name = quantities_plot_auto,
    command = powerscale_seq_summ_plot(sensitivity_sequence_auto)
  ),
  tar_target(
    sensitivity_jointplot,
    join_plots(sensitivity_plot_base, sensitivity_plot_auto, type = "ecdf")
  ),
  tar_target(
    sensitivity_jointplot_tikz,
    save_tikz_plot(sensitivity_jointplot, "../../figs/bodyfat_sens_joint.tex", width = 5.2, 3.2)
  ),
  tar_target(
    posterior_plot_base_tikz,
    save_tikz_plot(posterior_plot_base, "../../figs/bodyfat_posterior_base.tex", 3, 3)
  ),
  tar_target(
    quantities_jointplot,
    join_plots(quantities_plot_base,
               quantities_plot_auto, type = "quantities")
  ),
  tar_target(
    quantities_plot_joint_tikz,
    save_tikz_plot(quantities_jointplot, "../../figs/bodyfat_quantities_joint.tex", 5.2, 4.5)
  )
)
