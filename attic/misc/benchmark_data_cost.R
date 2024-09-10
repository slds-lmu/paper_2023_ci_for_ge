library(mlr3db)
library(mlr3)

path = file.path("/gscratch", "sfische6", "ci_for_ge_data", paste0("simulated_adult", ".parquet"))

backend = as_duckdb_backend(path)

n = 100

data = backend$data(1:n, backend$colnames)

backend_dt = as_data_backend(data)

bench::mark(
    duckdb = backend$data(1:n, backend$colnames),
    dt     = backend_dt$data(1:n, backend_dt$colnames),
    check = FALSE
)

