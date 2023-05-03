library(targets)
library(stantargets)

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
    "cowplot",
    "loo",
    "posterior",
    "brms",
    "lme4",
    "viridis",
    "bayesplot",
    "patchwork",
    "priorsense",
    "tikzDevice",
    "cmdstanr"
  ),
  imports = "priorsense"
)

list(
  tar_target(
    full_data,
    get_data()
  ),
  tar_target(
    sub_data,
    full_data[-(seq(1, 133, by = 3)), ]
  ),
  # full data
  ## base fit
  tar_stan_mcmc(
    name = fit_base_fulldata,
    stan_files = "stan/gpbffg_diffprior.stan",
    data = prepare_data(full_data, sigma_f_s = 0.05, prior_alpha = 1, likelihood_alpha = 1),
    include_paths = "stan",
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123,
    chains = 4,
    parallel_chains = 4
  ),
  tar_target(
    base_prior_plotdata_fulldata,
    prep_plot(fit_base_fulldata_mcmc_gpbffg_diffprior, full_data, component = "prior",  1)
  ),
  tar_target(
    base_lik_plotdata_fulldata,
    prep_plot(fit_base_fulldata_mcmc_gpbffg_diffprior, full_data, component = "likelihood",  1)
  ),
  tar_target(
    adjust_prior_plotdata_fulldata,
    prep_plot(fit_adjust_fulldata_mcmc_gpbffg_diffprior, full_data, component = "prior",  1)
  ),
  tar_target(
    adjust_lik_plotdata_fulldata,
    prep_plot(fit_adjust_fulldata_mcmc_gpbffg_diffprior, full_data, component = "likelihood",  1)
  ),

  ## adjusted fit
  tar_stan_mcmc(
    name = fit_adjust_fulldata,
    stan_files = "stan/gpbffg_diffprior.stan",
    data = prepare_data(full_data, sigma_f_s = 0.1, prior_alpha = 1, likelihood_alpha = 1),
    include_paths = "stan",
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 456,
    chains = 4,
    parallel_chains = 4
  ),
  tar_target(
    adjust_plotdata_fulldata,
    prep_plot(fit_adjust_fulldata_mcmc_gpbffg_diffprior, full_data, component = "base",  1)
  ),
    ## weak prior mcmc fit
  ## tar_stan_mcmc(
  ##   name = fit_weakprior_fulldata,
  ##   stan_files = "stan/gpbffg_diffprior.stan",
  ##   data = prepare_data(full_data, prior_alpha = 0.9, likelihood_alpha = 1),
  ##   include_paths = "stan",
  ##   iter_warmup = 1000,
  ##   iter_sampling = 1000,
  ##   adapt_delta = 0.999,
  ##   max_treedepth = 15,
  ##   seed = 123,
  ##   chains = 4,
  ##   parallel_chains = 4
  ## ),
  ## tar_target(
  ##   weakprior_plotdata_fulldata,
  ##   prep_plot(fit_weakprior_fulldata_mcmc_gpbffg_diffprior, full_data, component = "prior",  0.9)
  ## ),
  ##   ## strong prior
  ## tar_stan_mcmc(
  ##   name = fit_strongprior_fulldata,
  ##   stan_files = "stan/gpbffg_diffprior.stan",
  ##   data = prepare_data(full_data, prior_alpha = 1/0.9, likelihood_alpha = 1),
  ##   include_paths = "stan",
  ##   iter_warmup = 1000,
  ##   iter_sampling = 1000,
  ##   adapt_delta = 0.999,
  ##   max_treedepth = 15,
  ##   seed = 123,
  ##   chains = 4,
  ##   parallel_chains = 4
  ## ),
  ## tar_target(
  ##   strongprior_plotdata_fulldata,
  ##   prep_plot(fit_strongprior_fulldata_mcmc_gpbffg_diffprior, full_data, component = "prior",  1/0.9)
  ## ),
  # calculate alphas
  ## tar_target(
  ##   lik_alpha_low_fulldata,
  ##   priorsense:::find_alpha_threshold(fit_base_fulldata_mcmc_gpbffg_diffprior, component = "likelihood",
  ##                        alpha_bound = 0.5)
  ## ),
  ## tar_target(
  ##   lik_alpha_high_fulldata,
  ##   priorsense:::find_alpha_threshold(fit_base_fulldata_mcmc_gpbffg_diffprior, component = "likelihood",
  ##                        alpha_bound = 2)
  ## ),
  ## tar_target(
  ##   prior_alpha_high_fulldata,
  ##   priorsense:::find_alpha_threshold(fit_base_fulldata_mcmc_gpbffg_diffprior, component = "prior",
  ##                        alpha_bound = 2)
  ## ),
  ## tar_target(
  ##   prior_alpha_low_fulldata,
  ##   priorsense:::find_alpha_threshold(fit_base_fulldata_mcmc_gpbffg_diffprior, component = "prior",
  ##                        alpha_bound = 0.5)
  ## ),
  ## tar_target(
  ##   mm_thresholds,
  ##   mm_check_sensitivity(
  ##     "stan/gpbffg_diffprior.stan",
  ##     prepare_data(full_data, sigma_f_s = 0.05, prior_alpha = 1, likelihood_alpha = 1)
  ##   )
  ## ),


  ## strong likelihood
  tar_target(
    lik_strong_is_fulldata,
    powerscale(
      fit_base_fulldata_mcmc_gpbffg_diffprior,
      alpha = 1/0.9,
      component = "likelihood"
    )
  ),
  tar_target(
    lik_strong_is_plotdata_fulldata,
    prep_plot(
      fit = lik_strong_is_fulldata,
      data = full_data,
      component = "likelihood",
      alpha_scale = 1/0.9
    )
  ),
  ## weak likelihood
  tar_target(
    lik_weak_is_fulldata,
    powerscale(
      fit_base_fulldata_mcmc_gpbffg_diffprior,
      alpha = 0.9,
      component = "likelihood"
    )
  ),

  ## strong likelihood (adjust)
  tar_target(
    lik_strong_is_adjust_fulldata,
    powerscale(
      fit_adjust_fulldata_mcmc_gpbffg_diffprior,
      alpha = 1/0.9,
      component = "likelihood"
    )
  ),
  tar_target(
    lik_strong_is_plotdata_adjust_fulldata,
    prep_plot(
      fit = lik_strong_is_adjust_fulldata,
      data = full_data,
      component = "likelihood",
      alpha_scale = 1/0.9
    )
  ),
  ## weak likelihood (adjust)
  tar_target(
    lik_weak_is_adjust_fulldata,
    powerscale(
      fit_adjust_fulldata_mcmc_gpbffg_diffprior,
      alpha = 0.9,
      component = "likelihood"
    )
  ),

  tar_target(
    lik_weak_is_plotdata_fulldata,
    prep_plot(lik_weak_is_fulldata, full_data, "likelihood", 0.9)
  ),

  tar_target(
    lik_weak_is_plotdata_adjust_fulldata,
    prep_plot(lik_weak_is_adjust_fulldata, full_data, "likelihood", 0.9)
  ),


  ### moment match
    tar_target(
    lik_weak_ismm_fulldata,
    powerscale_mm(
    stan_files = "stan/gpbffg_diffprior.stan",
    data = prepare_data(full_data, sigma_f_s = 0.05, prior_alpha = 1, likelihood_alpha = 1),
    include_paths = "stan",
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    component = "likelihood",
    alpha = 0.9
    )
  ),
  tar_target(
    lik_weak_ismm_plotdata_fulldata,
    prep_plot(lik_weak_ismm_fulldata, full_data, "likelihood", 0.9)
  ),
  ## strong prior
  tar_target(
    prior_strong_is_fulldata,
    powerscale(
      fit_base_fulldata_mcmc_gpbffg_diffprior,
      alpha = 1/0.9,
      component = "prior"
    )
  ),
  tar_target(
    prior_strong_is_plotdata_fulldata,
    prep_plot(
      prior_strong_is_fulldata,
      full_data,
      component = "prior",
      alpha_scale = 1/0.9
    )
  ),

  ## strong prior (adjust)
  tar_target(
    prior_strong_is_adjust_fulldata,
    powerscale(
      fit_adjust_fulldata_mcmc_gpbffg_diffprior,
      alpha = 1/0.9,
      component = "prior"
    )
  ),
  tar_target(
    prior_strong_is_plotdata_adjust_fulldata,
    prep_plot(
      prior_strong_is_adjust_fulldata,
      full_data,
      component = "prior",
      alpha_scale = 1/0.9
    )
  ),

  ## weak prior
  tar_target(
    prior_weak_is_fulldata,
    powerscale(
      fit_base_fulldata_mcmc_gpbffg_diffprior,
      alpha = 0.9,
      component = "prior"
    )
  ),
  tar_target(
    prior_weak_is_plotdata_fulldata,
    prep_plot(
      prior_weak_is_fulldata,
      full_data,
      "prior",
      0.9
    )
  ),
  ## weak prior (adjust)
  tar_target(
    prior_weak_is_adjust_fulldata,
    powerscale(
      fit_adjust_fulldata_mcmc_gpbffg_diffprior,
      alpha = 0.9,
      component = "prior"
    )
  ),
  tar_target(
    prior_weak_is_plotdata_adjust_fulldata,
    prep_plot(
      prior_weak_is_adjust_fulldata,
      full_data,
      "prior",
      0.9
    )
  ),
  ## joint plot
  tar_target(
    jointplot,
    plot_sense(
      list(
        prior_weak_is_plotdata_fulldata,
        base_prior_plotdata_fulldata,
        prior_strong_is_plotdata_fulldata,
        lik_weak_ismm_plotdata_fulldata,
        base_lik_plotdata_fulldata,
        lik_strong_is_plotdata_fulldata
      )
    )
  ),
  tar_target(
    jointplot_adjust,
    plot_sense(
      list(
        prior_weak_is_plotdata_adjust_fulldata,
        adjust_prior_plotdata_fulldata,
        prior_strong_is_plotdata_adjust_fulldata,
        lik_weak_is_plotdata_adjust_fulldata,
        adjust_lik_plotdata_fulldata,
        lik_strong_is_plotdata_adjust_fulldata
      )
    )
  ),
    ## joint plot
  ## tar_target(
  ##   jointplot_mcmc,
  ##   plot_sense(
  ##     list(
  ##       weakprior_plotdata_fulldata,
  ##       base_prior_plotdata_fulldata,
  ##       strongprior_plotdata_fulldata
  ##     )
  ##   )
  ## ),
  tar_target(
    motorcycle_sense_plot,
    save_tikz_plot(
      jointplot, "../../figs/motorcycle_sense_plot.tex",
      width = 379 / 72.27,
      height = 379 / 72.27
    )
  ),
    tar_target(
    motorcycle_adjust_sense_plot,
    save_tikz_plot(
      jointplot_adjust, "../../figs/motorcycle_sense_adjust_plot.tex",
      width = 379 / 72.27,
      height = 379 / 72.27
    )
    ),
  ## base plot
  tar_target(
    base_plot,
    plot_base(
      list(
        base_prior_plotdata_fulldata
      )
    )
  ),
  tar_target(
    motorcycle_base_plot,
    save_tikz_plot(
      base_plot, "../../figs/motorcycle_base_plot.tex",
      width = 379 / 72.27 / 1.5,
      height = 379 / 72.27
    )
  ),
  ## adjusted plot
  tar_target(
    adjust_plot,
    plot_base(
      list(
        adjust_plotdata_fulldata
      )
    )
  ),
  tar_target(
    motorcycle_adjust_plot,
    save_tikz_plot(
      adjust_plot, "../../figs/motorcycle_adjust_plot.tex",
      width = 379 / 72.27 / 1.5,
      height = 379 / 72.27
    )
  )
)
