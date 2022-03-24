library(targets)

# custom functions
source("R/functions.R")

# Set target-specific options such as packages.
tar_option_set(
  packages = c("tidyverse", "brms", "priorsense", "rstan", "cowplot", "tikzDevice"),
  imports = "priorsense"
)

# End this file with a list of target objects.
list(
  tar_target(
    name = hemo_data_file,
    command = "data/hemophilia.csv",
    format = "file"
  ),
  tar_target(
    name = hemo_stancode_file,
    command = "stan/hemophilia.stan",
    format = "file"
  ),
  tar_target(
    name = hemo_formula,
    command = create_formula()
  ),
  tar_target(
    hemo_cleandata,
    prepare_data(hemo_data_file)
  ),
  tar_target(
    prior_base,
    make_prior(b1_mu = 0, b2_mu = 0, b1_sigma = 10, b2_sigma = 10)
    ), 
  tar_target(
    hemo_fit_base,
    run_model(hemo_formula, hemo_cleandata, prior_base)
  ),
  tar_target(
    hemo_sensitivity_base,
    run_sensitivity_analysis(hemo_fit_base)
  ),
  tar_target(
    prior_wide,
    make_prior(b1_mu = 0, b2_mu = 0, b1_sigma = 100, b2_sigma = 100)
    ), 
  tar_target(
    hemo_fit_wide,
    run_model(hemo_formula, hemo_cleandata, prior_wide)
  ),
  tar_target(
    hemo_sensitivity_wide,
    run_sensitivity_analysis(hemo_fit_wide)
  ),
#  tar_target(
#    hemo_sensitivity2,
#    run_sensitivity_analysis(hemo_formula, hemo_cleandata, seed = 4321)
#  ),
#  tar_target(
#    hemo_sensitivity_sequence,
#    run_sensitivity_sequence(hemo_formula, hemo_cleandata)
#  ),
  tar_target(
    hemo_plot,
    create_plot(hemo_cleandata)
  ),
  tar_target(
    hemo_tikz_plot,
    save_tikz_plot(hemo_plot, "../../figs/hemophilia_plot.tex", 3, 3)
  )
)
