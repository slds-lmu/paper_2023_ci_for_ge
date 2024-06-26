library(mlr3oml)
library(mlr3misc)
library(mlr3)
library(here)
library(DBI)
library(duckdb)
options(mlr3oml.parquet = TRUE)

n_subsets = 500L
sizes = c(100L, 500L, 1000L, 5000L, 10000L)

data_ids = unname(unlist(list(
  classif = c(45570, 45689, 45704, 45654, 45665, 45668, 45669, 45672, 45693),
  regr = c(45692, 45694, 45655, 45666, 45667, 45670, 45671, 45695, 45696)
)))

write_parquet = function(data, path) {
  con = dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "ids", data, row.names = FALSE)
  dbExecute(con, sprintf("COPY ids TO '%s.parquet' (FORMAT PARQUET);", path))
  dbDisconnect(con, shutdown = TRUE)
}

splits = lapply(data_ids, function(data_id) {
  print(data_id)
  if (!dir.exists(here("data", "splits", data_id))) {
    dir.create(here("data", "splits", data_id), recursive = TRUE)
  } else {
    return(NULL)
  }
  set.seed(data_id)
  task = as_task(odt(data_id))
  # when sampling the large subsets there is no need to stratify as it is large enough
  holdout_ids = sample(task$row_ids, 100000L)
  if (inherits(task, "TaskClassif")) {
    task$col_roles$stratum = task$target_names
  }
  write_parquet(data.frame(row_id = holdout_ids), here("data", "splits", data_id, "holdout_100000"))

  partition_ids = setdiff(task$row_roles$use, holdout_ids)
  lapply(sizes, function(size) {
    set.seed(data_id + size)
    print(size)
    # subsample enough data to be able to create exactly `n_subsets` subsets of size `size`
    task$row_roles$use = sample(partition_ids, size * n_subsets)
    resampling = rsmp("cv", folds = n_subsets)$instantiate(task)
    inst = resampling$instance

    names(inst) = c("row_id", "iter")
    inst = inst[, c("iter", "row_id")]

    # test sets don't have the same size when stratifying, so we correct this here
    if (inherits(task, "TaskClassif")) {
      # all tests sets have at least (size -1) obs,
      # so we keep those and then evenly split the remaining observations to all folds
      first = inst[, .SD[1:(size - 1), ], by = "iter"]
      rest = inst[!list(first$row_id), on = "row_id"]
      stopifnot(nrow(rest) == 500)
      rest$iter = 1:500

      inst = rbind(first, rest)
    }



    write_parquet(inst, here("data", "splits", data_id, paste0(size)))

    NULL
  })

  NULL
})
