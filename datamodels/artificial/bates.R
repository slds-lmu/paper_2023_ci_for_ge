library(here)
library(arrow)
library(data.table)
library(checkmate)
library(mlr3misc)

simulate_bates = function(n, p, task_type) {
  assert_int(n, lower = 1)
  assert_choice(p, c(20, 100))
  assert_choice(task_type, c("regr", "classif"))
  X = matrix(rnorm(n = n * p), nrow = n, ncol = p)
  beta = c(rep(1, 5), rep(0, p - 5))

  lp = X %*% beta

  X = as.data.table(X)
  colnames(X) = paste0("x", seq(p))

  if (task_type == "classif") {
    prob = as.numeric(1 / (1 + exp(-lp)))
    y = map_int(prob, function(p) sample(x = 0:1, prob = c(1 - p, p), size = 1))
    y = as.factor(y)
  } else if (task_type == "regr") {
    y = as.numeric(lp + rnorm(n))
  }

  dt = cbind(data.table(y = y), X)
  return(dt)
}

if (TRUE) {
  {
  set.seed(42)
  ns = 5100000
  ps = c(20, 100)
  task_types = c("classif", "regr")

  design = expand.grid(n = ns, p = ps, task_type = task_types, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(design))) {
    n = design[i, "n"]
    p = design[i, "p"]
    task_type = design[i, "task_type"]

    data = do.call(simulate_bates, args = list(p = p, n = n, task_type = task_type))
    name = paste("bates", task_type, p, sep = "_")
    pth = here::here("data", "artificial", paste0(name, ".pq"))
    arrow::write_parquet(data, pth)
  }
  }
}
