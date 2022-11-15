library(tidyverse)

out <- readRDS("separation_results.RDS")


sens <- list()
for (i in seq_along(out)) {
  current <- out[[i]]$sens$sensitivity
  current$iter <- i
  current$ncomplete <- out[[i]]$ncompletes
  sens <- c(sens, list(current))
}

sens <- bind_rows(sens)

sens |>
  mutate(
    variable = factor(variable)) |>
  pivot_longer(c(prior, likelihood), names_to = "component") |>
  ggplot(
  aes(x = ncomplete,
      y = value,
      colour = variable)) +
  geom_point() +
  facet_wrap(~component)
  

