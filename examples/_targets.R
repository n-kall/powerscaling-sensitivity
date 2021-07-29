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
source("R/analytical_scaling.R")
source("R/conflict.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "latex2exp", "scales", "tikzDevice",
                            "cmdstanr", "priorsense", "posterior", "cowplot",
                            "patchwork", "metRology", "stringi"))

list(
  # example dists
  tar_target(
    name = parameters,
    command = set_parameter_values()
  ),
  tar_target(
    distributions,
    do.call(create_distributions, parameters)
  ),
  tar_target(
    plot,
    make_example_dists_plot(distributions)
  ),
  tar_target(
    tikzplot,
    save_plot(plot, "../figs/example_dists.tex", width = 6 , height = 2)
  ),
  # conflict examples
  tar_target(
    conflict_model_file,
    "stan/conflict_normal_student-t.stan",
    format = "file"
  ),
  tar_target(
    conflict_model,
    cmdstan_model(conflict_model_file)
  ),
  tar_target(
    n_prior_n_lik_draws,
    normal_prior_normal_lik(conflict_model, 200000, 2000)
  ),
  tar_target(
    scale_example_plot,
    scaling_example_plot(n_prior_n_lik_draws)
  ),
  tar_target(
    scale_example_tikz,
    save_plot(scale_example_plot, "../figs/scaling-example.tex", 6, 1.8)
  ),
  tar_target(
    n_prior_n_lik_plot,
    normal_prior_normal_lik_plot(n_prior_n_lik_draws)
  ),
  tar_target(
    n_prior_n_lik_tikz,
    save_plot(n_prior_n_lik_plot, "../figs/conflict-example-1.tex", 6, 3.2)
  ),
  tar_target(
    n_prior_t_lik_draws,
    normal_prior_t_lik(conflict_model, 200000, 2000)
  ),
  tar_target(
    n_prior_t_lik_plot,
    normal_prior_t_lik_plot(n_prior_n_lik_draws)
  ),
  tar_target(
    n_prior_t_lik_tikz,
    save_plot(n_prior_t_lik_plot, "../figs/conflict-example-2.tex", 6, 3.2)
  ),
  tar_target(
    weakly_inf_example_draws,
    weakly_inf_normal_prior_normal_lik(conflict_model, 200000, 2000)
  ),
  tar_target(
    weakly_inf_plot,
    weakly_inf_normal_prior_normal_lik_plot(weakly_inf_example_draws)
  ),
  tar_target(
    weakly_inf_tikz,
    save_plot(weakly_inf_plot, "../figs/weak.tex", 6, 3.2)
  )
)
