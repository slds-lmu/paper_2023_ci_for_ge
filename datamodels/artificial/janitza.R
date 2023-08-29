library(here)
library(arrow)
library(data.table)
library(checkmate)
library(mlr3misc)
library(mvtnorm)


simulate_janitza = function(n, C) {
  assert_matrix(C)
  assert_true(nrow(C) == ncol(C))
  assert_int(n, lower = 1)

  X = rmvnorm(n, sigma = C)

  possible_effect_sizes = c(-3, -2, -1, -0.5, 0, 0.5, 1, 2, 3)

  beta = sample(possible_effect_sizes, replace = TRUE, size = ncol(X))

  lp = scale(X) %*% beta

  probs = 1 / (1 + exp(-lp))

  y = map_int(probs, function(prob) {
    sample(x = 0:1, prob = c(1 - prob, prob), size = 1)
  })
  y = as.factor(y)

  X_dt = as.data.table(X)
  colnames(X_dt) = paste0("x", seq_len(ncol(X_dt)))

  dt = cbind(data.table(y = y), X_dt)

  return(dt)
}

if (TRUE) {
  {
  set.seed(42)
  n = 5100000
  colon = read.table(here("data", "original", "covariance", "colon.data.txt"))
  colon = colon[, -1]
  cov_colon = cov(colon)
  colon = simulate_janitza(n, cov_colon)
  write_parquet(colon, here("data", "simulated", "colon.pq"))
  rm(colon)

  set.seed(43)
  breast2 = read.table(here("data", "original", "covariance", "breast.2.class.data.txt"))
  breast2 = breast2[, -1]
  cov_breast = cov(breast2)
  breast = simulate_janitza(n, cov_breast)
  write_parquet(breast, here("data", "artificial", "breast.pq"))
  rm(breast)

  set.seed(44)
  prostate = read.table(here("data", "original", "covariance", "prostate.data.txt"))
  prostate = prostate[, -1]
  cov_prostate = cov(prostate)
  prostate = simulate_janitza(n, cov_prostate)
  write_parquet(prostate, here("data", "simulated", "prostate.pq"))
  rm(prostate)
  }
}


