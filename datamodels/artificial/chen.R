library(here)
library(arrow)
library(data.table)
library(checkmate)
library(mlr3misc)

simulate_chen = function(n, M, null = FALSE) {
  assert_int(n, lower = 1)
  assert_choice(M, c(10, 30))
  assert_flag(null)

  vs = lapply(1:6, function(i) runif(n))

  f = function(a, b, c) {
    0.25 * exp(4 * a) + 4 / (1 + exp(-20 * (b - 0.5))) + 3 * c + rnorm(n, sd = 0.2)
  }

  if (!null) {
    y = f(vs[[1]], vs[[2]], vs[[3]])
  } else {
    y = f(a = rnorm(n, 0.2), b = rnorm(n, 0.2), c = rnorm(n, 0.2))
  }


  x_all = lapply(1:6, function(j) {
    xs = lapply(seq_len(M), function(m) {
      vs[[j]] + (0.01 + 0.5 * (m - 1) / (M - 1)) * rnorm(n, 0.3)
    })
    xs = as.data.table(xs)
    colnames(xs) = paste0("x", j, seq_len(M))
    xs
  })

  xs = Reduce(cbind, x_all)

  dt = cbind(data.table(y = y), xs)
}

if (FALSE) {
  {
  set.seed(42)
  ns = 5100000
  Ms = 10
  nulls = c(FALSE, TRUE)

  design = expand.grid(n = ns, M = Ms, null = nulls, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(design))) {
    n = design[i, "n"]
    M = design[i, "M"]
    null = design[i, "null"]
    data = do.call(simulate_chen, args = list(n = n, M = M, null = null))

    name = paste("chen", M, sep = "_")
    if (null) name = paste0(name, "_null")
    path = here::here("data", "artificial", paste0(name, ".pq"))

    write_parquet(data, path)
  }
  }
}
