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
source("R/cdf_diff.R")

#global vars
NSAMP <- 2e5
NWARM <- 2000

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "latex2exp", "scales", "tikzDevice",
                            "cmdstanr", "priorsense", "posterior", "cowplot",
                            "patchwork", "metRology", "stringi", "viridisLite"))

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
    analytical_plot,
    make_example_dists_plot(distributions)
  ),
  tar_target(
    analytical_tikzplot,
    save_plot(analytical_plot, "../figs/example_dists.tex", width = 5.2 , height = 5.2)
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
    normal_prior_normal_lik(conflict_model, NSAMP, NWARM)
  ),
  tar_target(
    scale_example_plot,
    scaling_example_plot(n_prior_n_lik_draws)
  ),
  tar_target(
    scale_example_tikz,
    save_plot(scale_example_plot, "../figs/scaling-example.tex", 379 / 72.27, 379 / 72.27)
  ),
  tar_target(
    n_prior_n_lik_plot,
    normal_prior_normal_lik_plot(n_prior_n_lik_draws)
  ),
  tar_target(
    n_prior_n_lik_tikz,
    save_plot(n_prior_n_lik_plot, "../figs/conflict-example-1.tex", 379 / 72.27, 379 / 72.27)
  ),
  tar_target(
    n_prior_t_lik_draws,
    normal_prior_t_lik(conflict_model, NSAMP, NWARM)
  ),
  tar_target(
    n_prior_t_lik_plot,
    normal_prior_t_lik_plot(n_prior_t_lik_draws)
  ),
  tar_target(
    n_prior_t_lik_tikz,
    save_plot(n_prior_t_lik_plot, "../figs/conflict-example-2.tex", 379 / 72.27, 379 / 72.27)
  ),
    tar_target(
    t_prior_t_lik_draws,
    student_prior_t_lik(conflict_model, NSAMP, NWARM)
  ),
  tar_target(
    t_prior_t_lik_plot,
    student_prior_t_lik_plot(t_prior_t_lik_draws)
  ),
  tar_target(
    t_prior_t_lik_tikz,
    save_plot(t_prior_t_lik_plot, "../figs/conflict-example-t_t.tex", 379 / 72.27, 379 / 72.27)
  ),
  tar_target(
    weakly_inf_example_draws,
    weakly_inf_normal_prior_normal_lik(conflict_model, NSAMP, NWARM)
  ),
  tar_target(
    weakly_inf_plot,
    weakly_inf_normal_prior_normal_lik_plot(weakly_inf_example_draws)
  ),
  tar_target(
    weakly_inf_tikz,
    save_plot(weakly_inf_plot, "../figs/weak.tex", 379 / 72.27, 379 / 72.27)
  ),

  # cdf diff example
  tar_target(
    cdf_example_plot,
    create_cdf_example_plot()
  ),
  tar_target(
    cdf_example_plot_tikz,
    save_plot(cdf_example_plot, "../figs/cdf_example.tex", 3.6, 3.6)
  )
)
