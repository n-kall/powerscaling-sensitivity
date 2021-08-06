prepare_data <- function(data_file) {
  read_delim(
    data_file, col_types = list(siri = "n"),
    delim = ";"
  ) %>%
    filter(siri > 0)
}

create_formula <- function() {
  bf(siri ~ age + weight_kg + height_cm + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, family = "gaussian")
}

prepare_standata <- function(formula, data) {
  make_standata(formula = formula, data = data)
}

run_model <- function(compiled_model, data, args) {
  fit1 <- sampling(
    compiled_model,
    iter = 2000,
    warmup = 1000,
    data = c(data, args),
    seed = 1234
  )
}

create_brm_model <- function(formula, data, model_fit, args) {

  bm <- brm(
    formula = formula,
    data = data,
    family = "gaussian",
    empty = TRUE,
    save_pars = save_pars(all = TRUE),
    stanvars = c(
      stanvar(x = args[[1]], name = "prior_width"),
      stanvar(x = args[["auto_prior"]], name = "auto_prior")
    )
  )

  bm$fit <- model_fit
  bm <- rename_pars(bm)
}

sensitivity_analysis <-  function(fit) {
  vars <- c("age", "weight_kg", "height_cm", "neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
  powerscale_sensitivity(fit, variables = c(paste0("b_", vars), "Intercept", "sigma"), component = c("prior", "likelihood"), log_prior_fn = extract_log_prior)
}

powerscale_seq <- function(fit) {  
  powerscale_sequence(fit, component = c("prior", "likelihood"), variables = "wrist", moment_match = FALSE, log_prior_fn = extract_log_prior)
}

powerscale_seq_mm <- function(auto_prior) {
f <- bf(siri ~ age + weight_kg + height_cm + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, family = "gaussian")

d <- read_delim("data/bodyfat.txt", col_types = list(siri = "n"), delim = ";") %>%
  filter(siri > 0)

stand <- make_standata(f, d)

m <- stan_model("stan/bodyfat.stan")

fit <- sampling(m, c(stand, auto_prior = auto_prior, prior_width = 1), iter = 2000, warmup = 1000, seed = 1234)

 bm <- brm(
    formula = f,
    data = d,
    family = "gaussian",
    empty = TRUE,
    save_pars = save_pars(all = TRUE),
    stanvars = c(
      stanvar(x = 1, name = "prior_width"),
      stanvar(x = 0, name = "auto_prior")
    )
  )

  bm$fit <- fit
  bm <- rename_pars(bm)


ps <- powerscale_sequence(
  bm,
  variables = c("b_wrist", "b_age", "b_chest"),
  component = c("prior", "likelihood"),
  moment_match = TRUE
)
}

powerscale_seq_plot <- function(powerscale_seq) {
  powerscale_plot_dens(powerscale_seq, variables = "b_wrist") +
    xlab("$\\beta^{\\text{wrist}}$") +
    guides(
      linetype = "none",
      color = guide_colorbar(
        title = "Power-scaling $\\alpha$"
      )
    ) +
    ggplot2::facet_grid(
      variable ~ component,
      labeller = ggplot2::labeller(
        component = c(
          likelihood = "Likelihood power-scaling",
          prior = "Prior power-scaling"
        )
      ),
      scales = "free"
    ) +
    cowplot::theme_half_open() +
    theme(
#      panel.background = element_rect(colour = "#F2F2F2",
#                                      fill = "#F2F2F2"),
      legend.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      strip.background = element_blank(),
      strip.text.y = element_blank(),
      strip.text.x = element_text(size = rel(0.6)),
      legend.text.align = 0,
      axis.line.y = element_line(colour = "gray"),
      axis.ticks.y = element_line(colour = "gray"),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray"),
      legend.title = element_text(size = rel(0.6))) +
    cowplot::panel_border()
}


powerscale_seq_summ_plot <- function(powerscale_seq) {

  powerscale_plot_quantities(powerscale_seq, variables = "b_wrist", quantities = c("mean", "median", "sd", "mad")) +
    facet_wrap(
      variable ~ quantity,
      scales = "free",
      ncol = 3,
      labeller = as_labeller(
        c(
          "b_wrist" = "",
          "sd" = "SD",
          "mean" = "Mean",
          "median" = "Median",
          "mad" = "MAD",
          "cjs_dist" = "$\\text{CJS}_{\\text{dist}}$"
        )
      )
    ) +
    guides(colour = "none") +
    xlab("Power-scaling $\\alpha$") +
    ylab("Value") +
    scale_color_manual(values = rep("black", 3)) +
    scale_shape_manual(values = c("prior" = 15, "likelihood" = 22), labels = c("Prior power-scaling", "Likelihood power-scaling"), name = "") + 
    cowplot::theme_half_open() +
    theme(
#      panel.background = element_rect(colour = "#F2F2F2",
#                                      fill = "#F2F2F2"),
      legend.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      strip.text = element_text(size = rel(0.6)),
      strip.background = element_blank(),
      legend.text.align = 0,
      axis.line.y = element_line(colour = "gray"),
      axis.ticks.y = element_line(colour = "gray"),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray"),
      legend.title = element_text(size = rel(0.6)),
      legend.position = c(0.65, 0.2),
    ) +
    cowplot::panel_border()
}

save_tikz_plot <- function(plot, filename, width, height) {
  
  tikz(file = filename, width = width, height = height)
  print(plot)

  dev.off()
}
