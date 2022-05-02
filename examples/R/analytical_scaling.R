dbeta_un <- function(x, a, b, s) {
  out <- x^(s * a - s + 1- 1) * (1 - x)^(s * b - s + 1 - 1)
  return(out)
}

dnorm_un <- function(x, mu, sigma, s) {
  out <- exp(s * -1/2 * ((x - mu) / sigma)^2)
  out
}

dexp_un <- function(x, lambda, s) {
  out <- s * lambda * exp(-s * lambda * x)
  out
}


dgamma_un <- function(x, shape, rate, s) {
  out <- x^(s * shape - s + 1 - 1) * exp(- s * rate * x)
  out
}

set_parameter_values <- function() {
  s <- c(0.5)
  s <- c(s, 1, rev(1/s))
  names(s) <- s
  a <- 2
  b <- 1
  mu <- 0
  sigma <- 1
  shape <- 2
  rate <- 2
  lambda <- 1

  list(
    s = s,
    a = a,
    b = b,
    mu = mu,
    sigma = sigma,
    shape = shape,
    rate = rate,
    lambda = lambda
  )
}


create_distributions <- function(s, a, b, mu, sigma, shape, rate, lambda) {

  d <- purrr::map_df(
    s,
    ~dbeta_un(
      x = seq(0, 1, 0.01),
      a = a,
      b = b,
      s = .x
    ) %>%
      bind_cols(theta = seq(0, 1, 0.01)),
    .id = "scale"
  ) %>%
    rename(density = `...1`) %>%
    mutate(dist = "$\\betadist(2,1)$")

  d <- d %>%
    bind_rows(
      purrr::map_df(
        s,
        ~dnorm_un(
          x = seq(-5, 5, 0.01),
          mu = mu,
          sigma = sigma,
          s = .x
        ) %>%
          bind_cols(theta = seq(-5, 5, 0.01)),
        .id = "scale"
      ) %>%
        rename(density = `...1`) %>%
        group_by(scale) %>%
        mutate(density = scale(density, center = FALSE),
               dist = "$\\normal(0, 1)$")
    )

  d <- d %>%
    bind_rows(
      purrr::map_df(
        s,
        ~dgamma_un(
          x = seq(0, 5, 0.01),
          shape = shape,
          rate = rate,
          s = .x
        ) %>%
          bind_cols(theta = seq(0, 5, 0.01)),
        .id = "scale"
      ) %>%
        rename(density = `...1`) %>%
        group_by(scale) %>%
        mutate(density = scale(density, center = FALSE),
               dist = "$\\gammadist(2, 2)$")
    )

  d <- d %>%
    bind_rows(
      purrr::map_df(
        s,
        ~dexp_un(
          x = seq(0, 5, 0.01),
          lambda = lambda,
          s = .x
        ) %>%
          bind_cols(theta = seq(0, 5, 0.01)),
        .id = "scale"
      ) %>%
        rename(density = `...1`) %>%
        group_by(scale) %>%
        mutate(density = scale(density, center = FALSE),
               dist = "$\\expdist(1)$")
    )

  return(d)
}

make_example_dists_plot <- function(d) {
  p <- d %>%
    mutate(
      scale = fct_recode(
        scale,
        "$\\alpha = 2.0$" = "2",
        "$\\alpha == 1.33$" = as.character(4/3),
        "$\\alpha = 1.0$" = "1",
        "$\\alpha = 0.75$" = "0.75",
        "$\\alpha = 0.5$" = "0.5"
      ),
      scale = fct_rev(scale),
      dist = fct_relevel(dist, "$\\expdist(1)$", "$\\normal(0, 1)$", "$\\betadist(2,1)$", "$\\gammadist(2, 2)$") 
    ) %>%
    group_by(scale, dist) %>%
    mutate(density = scale(density, center = FALSE)) %>%
    ggplot(aes(x = theta, y = density, color = scale)) +
    geom_line(size = 1) +
    ggplot2::scale_color_manual(
      values = cetcolor::cet_pal(3, "d8")) +
    ylab("Unnormalized density") +
    xlab("$\\theta$") +
    cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = c(0.05, 0.85),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
    ) +
    ylab("$p(\\theta)^{\\alpha}$") +
    cowplot::panel_border("black", size = 1) +
    facet_wrap(~dist, ncol = 4, scales = "free") +
    guides(color = guide_legend(keywidth = 0.2, keyheight = 0.15, default.unit = "inch"))

  return(p)
}

save_plot <- function(plot, filename, width, height) {
  
  tikz(file = filename, width = width, height = height)
  print(plot)

  dev.off()
  lines <- readLines(con = filename)
  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines))]
  writeLines(lines, con = filename)
}
