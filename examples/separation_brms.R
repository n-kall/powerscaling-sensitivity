library(tidyverse)
library(brms)
library(priorsense)
library(cmdstanr)
library(posterior)
library(detectseparation)
library(purrr)
library(patchwork)
library(ncomplete)

set.seed(123)

separate_gendata <- function(n = 15, intercept = 0, b = c(1, 1),
                             lim = 1) {
  X <- matrix(c(rep(1, n), runif(n * length(b), -lim, lim)),
              ncol = length(b) + 1)
  p <- plogis(rowSums(c(0, b) * X))
  y <- rbinom(n, 1, p)
  dat <- tibble(X1 = X[,1], X2 = X[,2], X3 = X[,3], Y = y)

  return(dat)
}

datasets <- map(
  1:3,
  ~separate_gendata(
    b = c(1, 1)
  )
)

ncompl <- function(dataset) {
  d <- as.matrix(dataset)
  ncomplete.for(d)

}

fits <- brm_multiple(
  Y ~ 1 + X1 + X2,
  data = datasets,
 family = "bernoulli",
 prior = c(
    prior(student_t(4, 0, 1), class = "b"),
    prior = prior(student_t(4, 0, 1), class = "Intercept")
  ),
  combin = FALSE,
  backend = "cmdstanr",
  refresh = 0
)

sens <- map(fits, ~powerscale_sensitivity(.x))

ncompletes <- map(datasets, ~ncompl(.x)$NCOMPLETE)

prior_sens <- map(sens, function(s) {s$sensitivity %>% select(prior) %>% max()})

like_sens <- map(sens, function(s) {s$sensitivity %>% select(likelihood) %>% min()})

pcomp <- qplot(unlist(ncompletes), unlist(prior_sens)) + geom_hline(yintercept = 0.05) + ylab("Prior sensitivity") + xlab("n_complete")

lcomp <- qplot(unlist(ncompletes), unlist(like_sens)) + geom_hline(yintercept = 0.05) + ylab("Likelihood sensitivity") + xlab("n_complete")

plt <- function(dataset) {

  x2 <- dataset$X2
  x3 <- dataset$X3
  y <- dataset$Y

  d <- tibble(x2 = x2, x3 = x3, y = y)

  d |>
    ggplot(
    aes(x = x2, y = x3, color = factor(y))) +
    geom_point() +
    coord_equal() +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_minimal() +
    scale_color_brewer(type = "qual", palette = "Set1")
}

plots <- map(datasets, ~plt(.x))

flagged <- map_lgl(sens, ~(!all(.x$sensitivity$diagnosis == "-")))
flagged_sep <- intersect(which(flagged), which(unlist(sep)))
flagged_nosep <- setdiff(which(flagged), which(unlist(sep)))
unflagged <- which(!flagged)

p <- wrap_plots(plots[c(unflagged[c(1)], flagged_sep[c(1)], flagged_nosep[c(1)])]) + plot_layout(guides = "collect", nrow = 1) + plot_annotation(tag_levels = "A")

ggsave("sep.pdf", p, width = 8, height = 6)
