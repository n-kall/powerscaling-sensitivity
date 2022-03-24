create_formula <- function() {
  bf(gr ~ AHFactivity1 + AHFactivity2, family = "bernoulli")
}

prepare_data <- function(data_file) {
  read_csv(data_file) %>%
    mutate(
      Group = factor(gr, levels = c("normal", "carrier"), labels = c("non-carrier", "carrier")),
      "AHFactivity1" = (AHFactivity - mean(AHFactivity)) / (2 * sd(AHFactivity)),
      "AHFactivity2" = (AHFactivity.1 - mean(AHFactivity.1)) / (2 * sd(AHFactivity.1))
    )
}

make_prior <- function(b1_mu, b2_mu, b1_sigma, b2_sigma) {
  prior <- c(
    prior_string(paste0("normal(", b1_mu, ", ", b1_sigma, ")"), coef = "AHFactivity1"),
    prior_string(paste0("normal(", b2_mu, ",", b2_sigma, ")"), coef = "AHFactivity2"),
    prior(normal(0, 100), class = "Intercept"))
  
  }

run_model <- function(formula, data, prior) {

  fit <- brm(formula, data, save_pars = save_pars(all = TRUE), prior = prior, iter = 2000, warmup = 1000, seed = 1234)
}

run_sensitivity_analysis <- function(fit) {
  list(
    ps_whiten = powerscale_sensitivity(
      fit,
      variable = "b", regex = TRUE,
      transform = "whiten"
    ),
    ps_orig = powerscale_sensitivity(
      fit,
      variable = "b", regex = TRUE
    )
  )
}

run_sensitivity_sequence <- function(formula, data, prior) {
  
  fit <- brm(formula, data, save_pars = save_pars(all = TRUE), prior = prior, iter = 2000, warmup = 1000, seed = 1234)

  list(
#    whitened = powerscale_sequence(fit, moment_match = FALSE, transform = "whiten"),
       orig = powerscale_sequence(fit)
       )
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
    geom_abline(intercept = -0.04, slope = 1.3, size = 0.45, linetype = "dashed", color = "gray") +    
    coord_equal() +
    xlim(-2.75, 2.5) +
    ylim(-2.75, 2.5) +
    scale_color_brewer(type = "qual") +
    xlab("AHF activity 1") +
    ylab("AHF activity 2") +
    cowplot::theme_half_open() +
    theme(
#      panel.background = element_rect(colour = "#F2F2F2",
#                                      fill = "#F2F2F2"),
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

