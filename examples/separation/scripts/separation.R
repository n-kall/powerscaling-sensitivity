library(priorsense)
library(cmdstanr)
library(posterior)
library(purrr)
library(furrr)
library(ncomplete)
library(dplyr)

#plan(multicore)

m_normal <- cmdstan_model(exe_file = "bernoulli_logit")

m_student <- cmdstan_model(exe_file = "bernoulli_logit_t")

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
  
  fit <- model$sample(data = data, refresh = 0, parallel_chains = 4, show_messages = FALSE)

  sens <- powerscale_sensitivity(fit)
  ncompletes <- ncompl(data)$NCOMPLETE

  out <- sens$sensitivity |>
    mutate(
      ncomplete = ncompletes,
      iter = iter
    )
  
  return(out)
  
}

iters <- 1:1000

normal_out <- map_dfr(
  iters,
  ~fit_sens(m_normal, iter = .x),
  .options = furrr_options(seed = 123)
)

student_out <- map_dfr(
  iters,
  ~fit_sens(m_student, iter = .x),
  .options = furrr_options(seed = 123)
)



saveRDS(normal_out, "separation_results_normal.RDS")

saveRDS(student_out, "separation_results_student.RDS")





#out <- readRDS("separation_results.RDS")

#sens |>
  ## gather(prior, likelihood, key = "component", value = "sensitivity") |>
  ## group_by(component, variable, ncomplete) |>
  ## summarise(mean_sens = mean(sensitivity)) |>
  ## ggplot(aes(x = ncomplete, y = mean_sens, group = variable)) +
  ## geom_line() +
  ## facet_wrap(~component)
