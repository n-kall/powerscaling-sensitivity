get_data <- function() {

  data(mcycle, package="MASS")
  mcycle
  
}

prepare_data <- function(data, sigma_f_s = 0.05, prior_alpha = 1, likelihood_alpha = 1, c_f = 1.5, M_f = 40, c_g = 1.5, M_g = 40) {

  mcycle <- data

  list(
    x = mcycle$times,
    y = mcycle$accel,
    N = length(mcycle$times),
    N2 = length(data$times),
    c_f = c_f,
    M_f = M_f,
    c_g = c_g,
    M_g = M_g,
    intercept_f_s = 1,
    intercept_g_s = 1,
    beta_f_s = 1,
    beta_g_s = 1,
    lscale_f_s = 1,
    lscale_g_s = 1,
    sigma_f_s = sigma_f_s,
    sigma_g_s = 0.5,
    prior_alpha = prior_alpha,
    likelihood_alpha = likelihood_alpha
  )
}

compile_model <- function(stan_file) {

  cmdstan_model(stan_file = stan_file, include_path = "stan")
  
}

fit_model <- function(model, standata) {

  fit_gpbffg <- model$sample(
    data=standata,
    iter_warmup=500, iter_sampling=1000, refresh=100,
    chains=4, parallel_chains=4, adapt_delta=0.99)

  fit
}


check_sensitivity <- function(psd) {

  powerscale_sensitivity(psd, variable = c("f"))

}


mm_check_sensitivity <- function(stan_file, standata) {

  m <- rstan::stan_model(stan_file, isystem = "stan")

  fit <- rstan::sampling(m, standata, iter = 2000, warmup = 500, seed = 123, control = list(adapt_delta = 0.9, max_treedepth = 13), cores = 4, refresh = 100)

  out <- list(
    prior_low = priorsense:::find_alpha_threshold(fit, "prior", c(0.5, 0.99), moment_match = TRUE),
    prior_high = priorsense:::find_alpha_threshold(fit, "prior", c(1.01, 2), moment_match = TRUE),
    lik_low = priorsense:::find_alpha_threshold(fit, "likelihood", c(0.5, 0.99), moment_match = TRUE),
    like_high = priorsense:::find_alpha_threshold(fit, "likelihood", c(1.01, 2), moment_match = TRUE)
  )

  return(out)
}

prep_plot <- function(fit, data, component, alpha_scale) {

  data(mcycle, package = "MASS")

  if (inherits(fit, "powerscaled_draws")) {
    dr <- as_draws_rvars(resample_draws(fit$draws))

  } else if (inherits(fit, "CmdStanFit")) {
    
    dr <- as_draws_rvars(fit$draws())

  } else {

    dr <- as_draws_rvars(fit)
  }
  
  data %>%
    mutate(Ef=mean(dr$f),
           sigma=mean(dr$sigma),
           component = component,
           alpha = paste0("Power-scaling $\\alpha$ = ", round(alpha_scale, 1)))
}

plot_base <- function(data) {

  linecolor <- RColorBrewer::brewer.pal(3, "Set1")[2]
  
  bind_rows(data) %>%  
    ggplot(aes(x=times,y=accel))+
    geom_point(alpha = 0.5, size = rel(0.8)) +
    labs(x = "Time (ms)", y = "Acceleration (g)") +
    geom_line(aes(y = Ef), color = linecolor, size = 1) +
    geom_line(aes(y = Ef - 1.96 * sigma), color = linecolor, size = 0.6) +
    geom_ribbon(aes(ymin = Ef - 0.67 * sigma, ymax = Ef + 0.65 * sigma), fill = linecolor, alpha = 0.3, color = NA) +
    geom_line(aes(y = Ef + 1.96 * sigma), color = linecolor, size = 0.6) +
    theme_cowplot() +
    scale_y_continuous(position = "right") +
    panel_border("black", size = 1) +
    ylim(-220, 140) +
    theme(
      aspect.ratio = 0.55,
      legend.text = element_text(size = rel(0.6)),
      legend.title = element_blank(),
      strip.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(colour = "black"),
      strip.background = element_blank()
    )
}


powerscale_mm <- function(
                          stan_files,
                          data,
                          include_paths,
                          iter_warmup,
                          iter_sampling,
                          adapt_delta,
                          max_treedepth,
                          seed,
                          component,
                          alpha,
                          chains,
                          parallel_chains
                          ) {

    m <- rstan::stan_model(stan_files, isystem = include_paths)

  fit <- rstan::sampling(m, data, iter = iter_warmup + iter_sampling, warmup = iter_warmup, seed = seed, chains = chains, cores = parallel_chains, refresh = 0, control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth))

  ps <- powerscale(fit, component = component, alpha = alpha, moment_match = TRUE)

  ps
}

plot_sense <- function(data) {

  linecolor <- RColorBrewer::brewer.pal(3, "Set1")[2]

  labels <- c(
    "prior" = "Prior\npower-scaling",
    "likelihood" = "Likelihood\npower-scaling"
  )
  
  bind_rows(data) %>%
    mutate(component = factor(component, levels = c("prior", "likelihood"))) %>%
    ggplot(aes(x = times, y = accel)) +
    labs(x="Time (ms)", y="Acceleration (g)") +
    geom_line(aes(y = Ef), color = linecolor, size = 1) +
    geom_line(aes(y = Ef - 1.96 * sigma), color = linecolor, size = 0.6) +
    geom_ribbon(aes(ymin = Ef - 0.67 * sigma, ymax = Ef + 0.65 * sigma), fill = linecolor, alpha = 0.3, color = NA) +
    geom_line(aes(y = Ef + 1.96 * sigma), color = linecolor, size = 0.6) +
    facet_grid(component ~ alpha, switch = "y", labeller = labeller(component = labels)) +
    scale_y_continuous(position = "right", limits = c(-220, 140)) +
    theme_cowplot() +
    panel_border("black", size = 1) +
    theme(
      aspect.ratio = 0.6,
      legend.text = element_text(size = rel(0.6)),
      legend.title = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      strip.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(colour = "black"),
      strip.background = element_blank()
    )
}


plot_raw <- function(d) {

  d %>%
    gather(-Status, key = Measure, value = Value) %>%
    mutate(Measure = factor(Measure, levels = c("Length", "Left", "Right", "Bottom", "Top", "Diagonal"))) %>%
    ggplot(aes(x = Value, fill = Status)) +
    facet_wrap(~Measure, scales = "free") +
    geom_density(alpha = 0.5) +
    theme_cowplot() +
    scale_fill_brewer(type = "qual") +
    xlab("Value (mm)") +
    ylab("") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "gray"),
          axis.ticks.x = element_line(color = "gray"),
          strip.background = element_blank(),          
          ) +
    panel_border()

}

save_tikz_plot <- function(plot, filename, width, height) {
  
  tikz(file = filename, width = width, height = height)
  print(plot)

  dev.off()
  lines <- readLines(con = filename)
  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines))]
  writeLines(lines, con = filename)
}
