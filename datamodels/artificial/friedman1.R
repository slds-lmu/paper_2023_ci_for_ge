library(here)
library(arrow)
library(data.table)
library(checkmate)
library(mlr3misc)
library(mlr3)


if (TRUE) {
  {
  set.seed(42)
  gen = tgen("friedman1")

  n = 5100000

  task = gen$generate(n = n)
  data = task$data()

  name = paste("friedman1")
  path = here::here("data", "artificial", paste0(name, ".parquet"))
  arrow::write_parquet(data, path)
  }
}
