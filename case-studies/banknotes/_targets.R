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
    name = formula,
    command = create_formula()
  ),
  tar_target(
    name = cleandata,
    command = prepare_data()
  ),
  tar_target(
    name = brm_base,
    command = create_brm_model(formula, cleandata, "base")
  ),
  tar_target(
   name = brm_wide,
    command = create_brm_model(formula, cleandata, "wide")
  ),
  tar_target(
    name = sensitivity_base,
    command = sensitivity_analysis(brm_base)
  ),
  tar_target(
    name = sensitivity_wide,
    command = sensitivity_analysis(brm_wide)
  )
)
