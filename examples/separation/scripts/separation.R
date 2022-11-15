library(priorsense)
library(cmdstanr)
library(posterior)
library(purrr)
library(furrr)
library(ncomplete)

plan(cluster)

m <- cmdstan_model(exe_file = "bernoulli_logit")

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

ncompl <- function(dataset) {

  d <- matrix(c(dataset$X[, -1], dataset$Y), nrow = nrow(dataset$X))
  ncomplete.for(d)

}

fit_sens <- function(model, iter) {

  data <- separate_gendata(
    b = c(1, 1),
    sigma = c(10, 10, 10)
  )
  
  fit <- model$sample(data = data, refresh = 0)

  sens <- powerscale_sensitivity(fit)
  ncompletes <- ncompl(data)$NCOMPLETE

  return(list(iter = iter, fit, sens = sens, ncompletes = ncompletes))
  
}

iters <- 1:10

out <- future_map(
  iters,
  ~fit_sens(m, iter = .x),
  .options = furrr_options(seed = 123)
)

saveRDS(out, "separation_results.RDS")

