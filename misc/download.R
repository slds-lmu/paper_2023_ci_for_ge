library(mlr3oml)

for (i in 1:100) {
  res = try(odt(45668, parquet = TRUE)$parquet_path)
  if (!inherits(res, "try-error")) {
    break
  }
}
