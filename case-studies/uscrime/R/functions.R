library(brms)
library(priorsense)
library(ggplot2)
library(ggdist)
library(dplyr)
library(posterior)
library(tikzDevice)

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

load_data <- function() {

  data(UScrime, package = "MASS")
  UScrime[, -c(2, ncol(UScrime))] <- log(UScrime[, -c(2, ncol(UScrime))])

  UScrime
}

make_formula <- function(family) {
bf(y ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + GDP + Ineq + Prob + Time, family = family)
}

dat <- load_data()

f_orig <- brm(make_formula("lognormal"), iter = 3000, warmup = 1000, prior = prior(R2D2(mean_R2 = 0.3, prec_R2 = 10, cons_D2 = 0.5)), data = dat, seed = 123, backend = "cmdstanr")


loo_R2_pred <- function(x) draws_df(R2 = loo_R2(x, summary = F), .nchains = 4)


psens_orig <- powerscale_sensitivity(f_orig, prediction = function(x) draws_df(R2 = bayes_R2(x, summary = F), .nchains = 4))

filter(psens_orig$sensitivity, variable == "R2")

ps_orig <- powerscale_sequence(f_orig, prediction = function(x) draws_df(R2 = bayes_R2(x, summary = F), .nchains = 4))

p1 <- powerscale_plot_quantities(ps_orig, variables = c("R2"), quantities = "mean") + ggplot2::ggtitle("mean_R2 = 0.3, prec_R2 = 10", subtitle = NULL)

f_alt <- brm(make_formula("lognormal"), iter = 3000, warmup = 1000, prior = prior(R2D2(mean_R2 = 0.3, prec_R2 = 1.5, cons_D2 = 0.5)), data = load_data(), control = list(adapt_delta = 0.99), seed = 123, backend = "cmdstanr", adapt_delta = 0.99)

autop <- sjstats::auto_prior(make_formula("lognormal"), data = load_data(), gaussian = FALSE)

f_unif <- brm(make_formula("lognormal"), data = load_data(), control = list(adapt_delta = 0.99))

f_auto <- brm(make_formula("lognormal"), iter = 3000, warmup = 1000, data = load_data(), prior = autop, control = list(adapt_delta = 0.99), backend = "cmdstanr")

psens_alt <- powerscale_sensitivity(f_alt, prediction = function(x) draws_df(R2 = bayes_R2(x, summary = FALSE), .nchains = 4))

filter(psens_alt$sensitivity, variable == "R2")

ps_alt <- powerscale_sequence(f_alt,  prediction = function(x) draws_df(R2 = bayes_R2(x, summary = FALSE), .nchains = 4))

p2 <- powerscale_plot_quantities(ps_alt, variables = c("R2"), quantities = "mean") + ggplot2::ggtitle("mean_R2 = 0.3, prec_R2 = 1.5", subtitle = NULL)


pars_orig <- simstudy::betaGetShapes(0.3, 10)

pars_alt <- simstudy::betaGetShapes(0.3, 1.5)

plt <- ggplot(data = tibble(x = c(0, 1)), aes(x)) +
  geom_function(n = 10000, fun = dbeta, args = list(shape1 = pars_orig[[1]], shape2 = pars_orig[[2]]), aes(color = "$\\betadist(3, 7)$"), linewidth = 1) +
  geom_function(n = 10000, fun = dbeta, args = list(shape1 = pars_alt[[1]], shape2 = pars_alt[[2]]), aes(color = "$\\betadist(0.45, 1.05)$"), linewidth = 1) +
  scale_color_brewer(type = "qual", palette = "Set1", guide = guide_legend(title = NULL)) +
  xlim(0.001, 0.999) +
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
      legend.position = c(0.1, 0.8),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
    ) +
  ylab("$p(R^2)$") +
  xlab("$R^2$") +
  cowplot::panel_border("black", size = 0.5)

save_tikz_plot <- function(plot, filename, width, height) {
  
  tikz(file = filename, width = width, height = height)
  print(plot)

  dev.off()
  lines <- readLines(con = filename)
  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines))]
  writeLines(lines, con = filename)
}


save_tikz_plot(plt, "../../figs/r2d2prior.tex", width = 2, 2)
