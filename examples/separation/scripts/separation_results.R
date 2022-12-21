library(tidyverse)
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


out <- readRDS("separation_results_normal.RDS")


out_student <- readRDS("separation_results_student.RDS")

p <- out |>
  mutate(
    variable = factor(variable)) |>
  filter(variable != "lprior") |>
  pivot_longer(c(prior, likelihood), names_to = "component") |>
  group_by(variable, ncomplete, component) |>
  summarise(sens = mean(value)) |>
  ggplot(
  aes(x = ncomplete,
      y = sens,
      colour = variable)) +
  geom_line() +
  geom_point(shape = 15) +
  cowplot::theme_half_open() +
  facet_wrap(~component, labeller = ggplot2::labeller(
        component = c(
          likelihood = "Likelihood power-scaling",
          prior = "Prior power-scaling"
        ))) +
      theme(
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = c(0.1, 0.3),
      axis.line.y = element_blank(),
      legend.text.align = 0,
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      legend.title = element_blank(),
      aspect.ratio = 1
      ) +
  scale_color_brewer(type = "qual", labels = c("$\\beta_0$", "$\\beta_1$", "$\\beta_2$")) +
  cowplot::panel_border("black", size = 0.5) +
  ylab("Sensitivity ($D_{\\text{CJS}}$)") +
  xlab("Data separability ($n_{\\text{complete}}$)")
  
  
save_tikz_plot <- function(plot, filename, width, height) {
  
  tikz(file = filename, width = width, height = height)
  print(plot)

  dev.off()
  lines <- readLines(con = filename)
  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines))]
  writeLines(lines, con = filename)
}


save_tikz_plot(p, "../../../figs/separation_example.tex", width = 5.2, height = 2.5)



