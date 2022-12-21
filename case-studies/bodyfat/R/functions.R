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

create_brm_model <- function(formula, data, prior) {

  if (prior == "auto") {
    prior <- sjstats::auto_prior(formula, data, gaussian = TRUE)
  } else if (prior == "base") {
    prior <- brms::prior(normal(0, 1), class = "b")
  }
  
  bm <- brm(
    formula = formula,
    data = data,
    iter = 2000,
    warmup = 1000,
    seed = 12345,
    family = "gaussian",
    save_pars = save_pars(all = TRUE),
    prior = prior
  )

  return(bm)
}

sensitivity_analysis <-  function(fit) {
  vars <- c("age", "weight_kg", "height_cm", "neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")

  newlabel <- function(x) {
    str_remove(str_remove(paste0("$", str_replace(x, "b_", "\\\beta^{\\\text{"),"}}$"), "_kg"), "_cm")
  }
  
  powerscale_sensitivity(fit, variable = c(paste0("b_", vars)))$sensitivity %>%
                                                                arrange(desc(row_number())) %>%
                                                                mutate(variable = newlabel(variable))
}

sensitivity_sequence <- function(fit) {  
  powerscale_sequence(fit, variable = "wrist", moment_match = FALSE)
}

sensitivity_sequence_mm <- function(data, formula, prior) {

  fit <- create_brm_model(formula, data,  prior)

  vars <- c("age", "weight_kg", "height_cm", "neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")


  ps <- powerscale_sequence(
    fit,
    variables = vars,
    lower_alpha = 0.8,
    length = 5,
    moment_match = TRUE,
    component = c("prior", "likelihood"),
    symmetric = TRUE
  )

}

powerscale_seq_plot <- function(powerscale_seq) {
  powerscale_plot_ecdf(powerscale_seq, variables = "b_wrist") +
    xlab("$\\beta^{\\text{wrist}}$") +
    guides(
      linetype = "none",
      color = guide_colorbar(
        title = "Power-scaling $\\alpha$"
      )
    ) +
    ggplot2::facet_grid(
      component ~ variable,
      labeller = ggplot2::labeller(
        component = c(
          likelihood = "Likelihood\npower-scaling",
          prior = "Prior\npower-scaling"
        )
      ),
      scales = "free",
      switch = "y"
    ) +
    cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title = element_text(size = 10),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y.left = element_text(size = 10, angle = 0, hjust = 1),
      legend.text.align = 0,
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      legend.title = element_text(size = 10),
      aspect.ratio = 1
    ) +
    cowplot::panel_border(color = "black", size = 1)
}


powerscale_seq_summ_plot <- function(powerscale_seq) {

  powerscale_plot_quantities(powerscale_seq, variables = "b_wrist", quantities = c("mean", "sd"), mcse = TRUE) +
    facet_wrap(
      . ~ quantity,
      scales = "free",
      ncol = 3,
      labeller = as_labeller(
        c(
          "b_wrist" = "",
          "sd" = "SD",
          "mean" = "Mean",
          "cjs_dist" = "$\\text{CJS}_{\\text{dist}}$"
        )
      )
    ) +
    guides(colour = "none") +
    xlab("Power-scaling $\\alpha$") +
    scale_color_manual(values = rep("black", 3)) +
    scale_shape_manual(values = c("prior" = 15, "likelihood" = 22), labels = c("Prior power-scaling", "Likelihood power-scaling"), name = NULL) +
    scale_linetype_manual(values = "dashed", labels = "$\\pm2$ MCSE", name = NULL) +
    cowplot::theme_half_open() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.text.align = 0,
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(colour = "black"),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      legend.title = element_text(size = 10),
      aspect.ratio = 1
    ) +
    cowplot::panel_border(color = "black", size = 1)
}


powerscale_seq_ecdf_plot <- function(powerscale_seq) {

  powerscale_plot_ecdf(powerscale_seq, variables = "b_wrist") +
    guides(colour = "none") +
    xlab("Power-scaling $\\alpha$") +
    scale_color_viridis_c(values = rep("black", 3)) +
    scale_shape_manual(values = c("prior" = 15, "likelihood" = 22), labels = c("Prior power-scaling", "Likelihood power-scaling"), name = NULL) + 
    cowplot::theme_half_open() +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.text.align = 0,
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(colour = "black"),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      legend.title = element_text(size = rel(0.6)),
      legend.position = c(0.65, 0.2),
      aspect.ratio = 1
      ) +
    cowplot::panel_border(color = "black", size = 1)
}

posterior_plot <- function(fit) {

  newlabel <- function(x) {
    str_remove(str_remove(paste0("$", str_replace(x, "b_", "\\\\beta^{\\\\text{"),"}}$"), "_kg"), "_cm")
  }

  fit %>%
    as_draws_rvars() %>%
    gather_rvars(b_age, b_weight_kg, b_height_cm, b_neck, b_chest, b_abdomen, b_hip, b_thigh, b_knee, b_ankle, b_biceps, b_forearm, b_wrist) %>%
    ggplot(aes(y = .variable, dist = .value)) +
    stat_pointinterval(point_size = 1.25) +
    theme_cowplot() +
        scale_y_discrete(labels = newlabel) +
    theme(
      axis.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.text.align = 0,
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(colour = "black"),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.text.y = element_text(hjust = 0)
    ) +
    ylab("") +
    xlab("") +
    cowplot::panel_border(color = "black", size = 1)

}

join_plots <- function(plot1, plot2, type) {

  if (type == "ecdf") {
    (plot1 + ggtitle(label = "", subtitle = "Original prior") + theme(plot.title = element_blank(), plot.subtitle = element_text(size = 10))) + (plot2 + theme(plot.title = element_blank(), strip.text.y.left = element_blank(), plot.subtitle = element_text(size = 10)) + ggtitle(label = "", subtitle = "Adjusted prior")) + patchwork::plot_layout(guides = "collect")
  } else if (type == "quantities") {
    (plot1 + ggtitle(label = "", subtitle = "Original prior") + theme(plot.title = element_blank(), plot.subtitle = element_text(size = 10))) / (plot2 + ggtitle(label = "", subtitle = "Adjusted prior") + theme(plot.title = element_blank(), plot.subtitle = element_text(size = 10))) + patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")
  }

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


powerscale_seq_summ_plot_poster <- function(powerscale_seq) {

  powerscale_plot_quantities(powerscale_seq, variables = "b_wrist", quantities = c("mean"), mcse = FALSE) +
    facet_wrap(
      . ~ quantity,
      scales = "free",
      ncol = 3,
      labeller = as_labeller(
        c(
          "b_wrist" = "",
          "sd" = "SD",
          "mean" = "Mean",
          "cjs_dist" = "$\\text{CJS}_{\\text{dist}}$"
        )
      )
    ) +
    guides(colour = "none") +
    xlab("Power-scaling $\\alpha$") +
    scale_color_manual(values = rep("black", 3)) +
    scale_shape_manual(values = c("prior" = 15, "likelihood" = 22), labels = c("Prior power-scaling", "Likelihood power-scaling"), name = NULL) +
    scale_linetype_manual(values = "dashed", labels = "$\\pm2$ MCSE", name = NULL) +
    cowplot::theme_half_open() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.text.align = 0,
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(colour = "black"),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      legend.title = element_text(size = 10),
      aspect.ratio = 1
    ) +

    cowplot::panel_border(color = "black", size = 1)
}
