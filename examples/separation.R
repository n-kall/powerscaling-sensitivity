library(tidyverse)
library(brms)
library(priorsense)
library(cmdstanr)
library(posterior)
library(purrr)
library(furrr)
library(patchwork)
library(ncomplete)

m <- cmdstan_model(stan_exe = "bernoulli_logit")

separate_gendata <- function(n = 15, intercept = 0, b = c(1, 1),
                             sigma = c(5, 5, 5),
                             lim = 1) {
  X <- matrix(c(rep(1, n), runif(n * length(b), -lim, lim)),
              ncol = length(b) + 1)
  p <- plogis(rowSums(c(0, b) * X))
  y <- rbinom(n, 1, p)
  dat <- list(N = n, Y = y, X = X, K = ncol(X), sigma = sigma)

  return(dat)
}

datasets <- map(
  1:2,
  ~separate_gendata(
    b = c(1, 1),
    sigma = c(10, 10, 10)
  )
)

ncompl <- function(dataset) {

  d <- matrix(c(dataset$X[, -1], dataset$Y), nrow = nrow(dataset$X))
  ncomplete.for(d)

}

fits <- future_map(datasets, ~m$sample(data = .x, refresh = 0))

sens <- future_map(fits, ~powerscale_sensitivity(.x))

ncompletes <- map(datasets, ~ncompl(.x)$NCOMPLETE)

prior_sens <- map(sens, function(s) {s$sensitivity %>% select(prior) %>% max()})

like_sens <- map(sens, function(s) {s$sensitivity %>% select(likelihood) %>% min()})

pcomp <- qplot(unlist(ncompletes), unlist(prior_sens)) + geom_hline(yintercept = 0.05) + ylab("Prior sensitivity") + xlab("n_complete")

lcomp <- qplot(unlist(ncompletes), unlist(like_sens)) + geom_hline(yintercept = 0.05) + ylab("Likelihood sensitivity") + xlab("n_complete")

plt <- function(dataset) {

  x1 <- dataset$X[,2]
  x2 <- dataset$X[,3]
  y <- dataset$Y

  d <- tibble(x1 = x1, x2 = x2, y = y)
  
  d |>
    ggplot(
    aes(x = x1, y = x2, color = factor(y))) +
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
