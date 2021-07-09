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
    hemo_standata,
    make_standata(hemo_formula, hemo_cleandata)
  ),
  tar_target(
    hemo_compiled_model,
    stan_model(hemo_stancode_file)
  ),
  tar_target(
    hemo_fit,
    run_model(hemo_compiled_model, hemo_standata)
  ),
  tar_target(
    hemo_sensitivity,
    run_sensitivity_analysis(hemo_formula, hemo_cleandata)
  ),
  tar_target(
    hemo_sensitivity_sequence,
    run_sensitivity_sequence(hemo_formula, hemo_cleandata)
  ),
  tar_target(
    hemo_plot,
    create_plot(hemo_cleandata)
  ),
  tar_target(
    hemo_tikz_plot,
    save_tikz_plot(hemo_plot, "../../figs/hemophilia_plot.tex", 3, 3)
  )
)
