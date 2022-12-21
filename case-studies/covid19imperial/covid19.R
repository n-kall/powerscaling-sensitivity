library(cmdstanr)
library(posteriordb)
library(posterior)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tikzDevice)
library(patchwork)
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



devtools::load_all("~/repo/priorsense")

po <- posterior("ecdc0401-covid19imperial_v3")
dat <- pdb_data(po)

#m <- cmdstan_model("stan/covid19imperial_v3.stan")

#f <- m$sample(data = dat, parallel_chains = 4, chains = 4)

f <- readRDS("fitted_model.RDS")

# tau prior
p1 <- powerscale_sensitivity(f, variable = "E_deaths0", prior_selection = 1)

# phi prior
p2 <- powerscale_sensitivity(f, variable = "E_deaths0", prior_selection = 2)

# kappa prior
p3 <- powerscale_sensitivity(f, variable = "E_deaths0", prior_selection = 3)


ps_kappa <- powerscale_sequence(f, variable = "E_deaths0[50,3]", prior_selection = 3)

quantplot <- powerscale_plot_quantities(ps_kappa, variables = "E_deaths0[50,3]", quantities = c("mean", "sd"), mcse = TRUE) +
      facet_wrap(
      . ~ quantity,
      scales = "free",
      ncol = 3,
      labeller = as_labeller(
        c(
          "sd" = "SD",
          "mean" = "Mean",
          "cjs_dist" = "$\\text{CJS}_{\\text{dist}}$"
        )
      )
      ) +
  ggtitle(NULL, subtitle = NULL) +
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
    cowplot::panel_border(color = "black", size = 0.5)


# get intervention times

interventions <- tibble()
for (p in 1:6) {

  interv <- which(rowCumsums(dat$X[,,p]) == 1, arr.ind = TRUE)
  interv <- as_tibble(interv) |>
    rename(country = row, day = col) |>
    mutate(intervention = p)

  interventions <- bind_rows(interventions, interv)  
}



countries <- c("Denmark", "Italy", "Germany", "Spain", "United Kingdom", "France", "Norway", "Belgium", "Austria", "Sweden", "Switzerland", "Greece", "Portugal", "Netherlands")

names(countries) <- as.character(1:14)

plt <- p3$sensitivity |>
  mutate(parameter = "kappa") |>
  bind_rows(p2$sensitivity |> mutate(parameter = "phi")) |>
  bind_rows(p1$sensitivity |> mutate(parameter = "tau")) |>
  mutate(variable = str_remove(variable, c("E_deaths0\\["))) |>
  mutate(variable = str_remove(variable, c("\\]"))) |>
  separate(variable, c("day", "country")) |>
  mutate(day = as.numeric(day), country = as.integer(country)) |>
  ggplot(aes(x = day, color = parameter)) +
  scale_color_brewer(type = "qual", labels = c("$\\kappa$ prior", "$\\phi$ prior", "$\\tau$ prior")) +
  geom_line(aes(y = prior), linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  facet_wrap(~country, labeller = labeller(country = countries)) +
  ylab("Prior sensitivity ($D_{\\text{CJS}})$") +
  xlab("Day") +
  geom_vline(aes(xintercept = day), data = interventions |> filter(intervention == 4), alpha = 0.5) +
  cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = c(0.6, 0.125),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
    ) +
    cowplot::panel_border("black", size = 0.5)

prior_plt_subset <-  p3$sensitivity |>
  mutate(parameter = "kappa") |>
  bind_rows(p2$sensitivity |> mutate(parameter = "phi")) |>
  bind_rows(p1$sensitivity |> mutate(parameter = "tau")) |>
  mutate(variable = str_remove(variable, c("E_deaths0\\["))) |>
  mutate(variable = str_remove(variable, c("\\]"))) |>
  separate(variable, c("day", "country")) |>
  mutate(day = as.numeric(day), country = as.integer(country)) |>
  filter(country %in% c(2, 3, 5, 10)) |>
  ggplot(aes(x = day, color = parameter)) +
  scale_color_brewer(type = "qual", labels = c("$\\kappa$ prior", "$\\phi$ prior", "$\\tau$ prior")) +
  geom_line(aes(y = prior), linewidth = 1) +
  guides(color = guide_legend(keywidth = 0.15, default.unit = "inch")) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  facet_wrap(~country, labeller = labeller(country = countries), ncol = 4) +
  ylab("Prior sensitivity ($D_{\\text{CJS}})$") +
  xlab("Day") +
  geom_vline(aes(xintercept = day), data = interventions |> filter(intervention == 4, country %in% c(2, 3, 5, 10)), alpha = 0.5) +
  cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = c(0.825, 0.7),
      legend.spacing.y = unit(0.15, "cm"),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
    ) +
    cowplot::panel_border("black", size = 0.5)


lik_plt <- p3$sensitivity |>
  mutate(variable = str_remove(variable, c("E_deaths0\\["))) |>
  mutate(variable = str_remove(variable, c("\\]"))) |>
  separate(variable, c("day", "country")) |>
  mutate(day = as.numeric(day), country = as.integer(country)) |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = likelihood), linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  facet_wrap(~country, labeller = labeller(country = countries)) +
  geom_vline(aes(xintercept = day), data = interventions |> filter(intervention == 4), alpha = 0.5) +
  ylab("Likelihood sensitivity ($D_{\\text{CJS}})$") +
  xlab("Day") +
  cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = c(0.6, 0.125),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
    ) +
    cowplot::panel_border("black", size = 0.5)


lik_plt_subset <- p3$sensitivity |>
  mutate(variable = str_remove(variable, c("E_deaths0\\["))) |>
  mutate(variable = str_remove(variable, c("\\]"))) |>
  separate(variable, c("day", "country")) |>
  mutate(day = as.numeric(day), country = as.integer(country)) |>
  filter(country %in%  c(2, 3, 5, 10)) |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = likelihood), linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  facet_wrap(~country, labeller = labeller(country = countries), ncol = 4) +
  geom_vline(aes(xintercept = day), data = interventions |> filter(intervention == 4, country %in%  c(2, 3, 5, 10)), alpha = 0.5) +
  ylab("Likelihood sensitivity ($D_{\\text{CJS}})$") +
  xlab("Day") +
  cowplot::theme_half_open() +
    theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = c(0.6, 0.125),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
    ) +
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


save_tikz_plot(plt, "../../figs/covid_sens.tex", width = 5.2, height = 5.2)

save_tikz_plot(lik_plt, "../../figs/covid_liksens.tex", width = 5.2, height = 5.2)

save_tikz_plot(lik_plt_subset, "../../figs/covid_liksubsens.tex", width = 5.2, height = 3)

save_tikz_plot(prior_plt_subset, "../../figs/covid_priorsubsens.tex", width = 5.2, height = 3)

save_tikz_plot(quantplot, "../../figs/covid_quants.tex", width = 5.2, height = 5.2/2)

