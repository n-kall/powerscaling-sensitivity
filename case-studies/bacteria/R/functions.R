plot_priors <- function() {
  x <- seq(0, 10, by = 0.1)
  x2 <- seq(0, 40, by = 0.1)
  ig <- dgamma(x, 0.01, 0.01)
  nor <- dnorm(x, 0, 1)
  cau <- dcauchy(x, 0, 1)
  ig2 <- dgamma(x, 1, 2)
  ig3 <- dgamma(x2, 9, 0.5)

  priordata <- tibble(
    tau = x,
    "$\\gammadist(0.01, 0.01)$" = ig,
    "$\\normal^+(0, 1)$" = nor,
    "$\\Cauchy^+(0, 1)$" = cau,
    "$\\gammadist(1, 2)$" = ig2
  ) %>%
    gather(key = "dist", value = "density", -tau) %>%
    mutate(
      dist = factor(
        dist, levels = c(
          "$\\gammadist(0.01, 0.01)$",
          "$\\normal^+(0, 1)$",
          "$\\Cauchy^+(0, 1)$",
          "$\\gammadist(1, 2)$",
          "$\\gammadist(9, 0.5)$"
        )
      )
    )

p <- priordata %>%
    ggplot(aes(x = tau, y = density, color = dist)) +
    geom_line(size = 1) +
  scale_color_brewer(type = "qual", palette = "Dark2", name = "Distribution", drop = FALSE) +
    cowplot::theme_half_open() +
    ylab("Density") +
    xlab("$\\tau$") +
  theme(
#    panel.background = element_rect(colour = "#F2F2F2",
#                                    fill = "#F2F2F2"),
      legend.text = element_text(size = rel(0.6)),
      axis.text = element_text(size = rel(0.6)),
      axis.title = element_text(size = rel(0.6)),
      strip.text = element_text(size = rel(0.6)),
      strip.background = element_blank(),
      legend.text.align = 0,
      legend.position = c(0.05, 0.88),
      axis.line.y = element_line(colour = "gray"),
      axis.ticks.y = element_line(colour = "gray"),
      axis.line.x = element_line(colour = "gray"),
      axis.ticks.x = element_line(colour = "gray"),
      legend.title = element_text(size = rel(0.6))) +
    cowplot::panel_border()

  priordata2 <- tibble(
    tau = x2,
    "$\\gammadist(9, 0.5)$" = ig3
  ) %>%
    gather(key = "dist", value = "density", -tau) %>%
    mutate(
      dist = factor(
        dist, levels = c(
          "$\\gammadist(0.01, 0.01)$",
          "$\\normal^+(0, 1)$",
          "$\\Cauchy^+(0, 1)$",
          "$\\gammadist(1, 2)$",
          "$\\gammadist(9, 0.5)$"
        )
      )
    )
  p2 <-  priordata2 %>%
    ggplot(aes(x = tau, y = density, color = dist)) +
    geom_line(size = 1) +
    scale_color_brewer(type = "qual", palette = "Dark2", name = "Distribution", drop = FALSE) +
    cowplot::theme_half_open() +
    ylab("") +
    xlab("$\\tau$") +
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
      legend.title = element_text(size = rel(0.6))) +
    cowplot::panel_border() +
    guides(color = "none")

  
  return((p + p2) + patchwork::plot_layout(guides = "collect"))
}


prepare_data <- function() {
  data(bacteria, package = "MASS")

  bacteria_d <- bacteria %>%
    mutate(trtDrug = trt == "drug", trtDrugP = trt == "drug+",
           y = as.integer(y) - 1,
           ID = as.integer(ID)) %>%
    dplyr::select(y, week, trtDrug, trtDrugP, ID) %>%
    as.list()

  bacteria_d <- c(bacteria_d, list(N = length(bacteria_d$y), K = length(unique(bacteria_d$ID))))

  return(bacteria_d)

}

create_fit <- function(model, data, prior) {

  sampling(model, data = c(data, prior = prior), warmup = 2000, iter = 10000, seed = 1234)
  
}


sensitivity_analysis <- function(fit, ...) {
  v <- c("b_week", "b_trtDrug", "b_trtDrugP", "mu", "tau")
  powerscale_sensitivity(fit, variables = v, log_prior_fn = extract_log_prior, moment_match = TRUE, ...)

}

sensitivity_sequence <- function(model, data, prior) {
  fit <- create_fit(model, data, prior)
  v <- c("b_week", "b_trtDrug", "b_trtDrugP", "mu", "tau")
  powerscale_sequence(fit, variables = v, log_prior_fn = extract_log_prior,  moment_match = TRUE)
}  

save_tikz_plot <- function(plot, filename, width, height) {
  
  tikz(file = filename, width = width, height = height)
  print(plot)

  dev.off()
}











