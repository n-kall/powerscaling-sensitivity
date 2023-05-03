prepare_data <- function() {
  data(banknote, package = "mclust")
  out <- banknote
}

create_formula <- function() {
  bf(Status ~ Length + Left + Right + Bottom + Top + Diagonal, family = "bernoulli")
}

create_brm_model <- function(formula, data, prior) {

  if (prior == "base") {
    pr <- c(
      prior(normal(0, 2.5), class = "Intercept"),
      prior(normal(0, 6.64), coef = "Length", class = "b"),
      prior(normal(0, 6.92), coef = "Left", class = "b"),
      prior(normal(0, 6.19), coef = "Right", class = "b"),
      prior(normal(0, 1.73), coef = "Bottom", class = "b"),
      prior(normal(0, 3.11), coef = "Top", class = "b"),
      #    prior(normal(0, 6.92), coef = "Bottom", class = "b"),
      #    prior(normal(0, 12.44), coef = "Top", class = "b"), 
      prior(normal(0, 2.17), coef = "Diagonal", class = "b")
    )
  } else if (prior == "wide") {
    pr <- c(
      prior(normal(0, 2.5), class = "Intercept"),
      prior(normal(0, 6.64), coef = "Length", class = "b"),
      prior(normal(0, 6.92), coef = "Left", class = "b"),
      prior(normal(0, 6.19), coef = "Right", class = "b"),
      prior(normal(0, 6.92), coef = "Bottom", class = "b"),
      prior(normal(0, 12.44), coef = "Top", class = "b"), 
      prior(normal(0, 8.68), coef = "Diagonal", class = "b")
    )
  }
    
    bm <- brm(
      formula = formula,
      data = data,
      iter = 2000,
      warmup = 1000,
      seed = 1234,
      family = "gaussian",
      save_pars = save_pars(all = TRUE),
      prior = pr
    )
  }

  sensitivity_analysis <-  function(fit) {  
    powerscale_sensitivity(fit, variable = "b_", regex = TRUE)
  }
