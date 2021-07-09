create_formula <- function() {
  bf(gr ~ AHFactivity1 + AHFactivity2, family = "bernoulli")
}

prepare_data <- function(data_file) {
  read_csv(data_file) %>%
    mutate(
      Group = factor(gr, levels = c("normal", "carrier"), labels = c("non-carrier", "carrier")),
      "AHFactivity1" = scale(AHFactivity),
      "AHFactivity2" = scale(AHFactivity.1)
    )
}

run_model <- function(compiled_model, data) {
  sampling(
    compiled_model,
    data = data
  )
}

run_sensitivity_analysis <- function(formula, data) {

  prior <- c(prior(normal(0, 2.5), class = "b"),
             prior(normal(0, 10), class = "Intercept"))
  
  fit <- brm(formula, data, save_pars = save_pars(all = TRUE), prior = prior)
  
  powerscale_sensitivity(
    fit,
    moment_match = TRUE
  )
}

run_sensitivity_sequence <- function(formula, data) {
  prior <- c(prior(normal(0, 2.5), class = "b"),
             prior(normal(0, 10), class = "Intercept"))
  
  fit <- brm(formula, data, save_pars = save_pars(all = TRUE), prior = prior)

  powerscale_sequence(fit, moment_match = TRUE)
}

create_plot <- function(data) {
  ggplot(data) +
    geom_point(
      aes(
        x = AHFactivity1,
        y = AHFactivity2,
        color = Group,
        shape = Group
      ),
      size = 2) +
    geom_abline(intercept = -0.05, slope = 1.35, size = 0.45, linetype = "dashed", color = "gray") +    
    coord_equal() +
    xlim(-2.75, 2.5) +
    ylim(-2.75, 2.5) +
    scale_color_brewer(type = "qual") +
    xlab("AHF activity 1") +
    ylab("AHF activity 2") +
    cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = rel(0.6)),
      legend.title = element_blank(),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      strip.text = element_text(size = rel(0.6)),
      axis.line.y = element_line(colour = "gray"),
      legend.position = c(0.05, 0.88),
      axis.ticks.y = element_line(colour = "gray"),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray")
    ) +
    cowplot::panel_border()
}


save_tikz_plot <- function(plot, filename, width, height) {
  
  tikz(file = filename, width = width, height = height)
  print(plot)

  dev.off()
}

