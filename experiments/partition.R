library(mlr3oml)
library(mlr3misc)
library(mlr3)
library(here)
library(DBI)
library(duckdb)
options(mlr3oml.parquet = TRUE)
#options(mlr3oml.cache = TRUE)
n_data = 5100000L
n_subsets = 500L
sizes = c(50L, 100L, 500L, 1000L, 5000L, 10000L)

data_ids = unname(unlist(list(
  classif = c(45570, 45689, 45704, 45654, 45665, 45668, 45669, 45672, 45693),
  regr = c(45692, 45694, 45655, 45666, 45667, 45670, 45671, 45695, 45696)
)))

#walk(data_ids, function(id) odt(id)$data)

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
  # when sampling the large subsets there is no need to stratify
  holdout_ids = sample(seq_len(task$nrow), 100000L)
  partition_ids = setdiff(task$row_roles$use, holdout_ids)
  if (inherits(task, "TaskClassif")) {
    task$col_roles$stratum = task$target_names
  }
  write_parquet(data.frame(row_id = holdout_ids[1:50000]), here("data", "splits", data_id,  "holdout_50000"))
  write_parquet(data.frame(row_id = holdout_ids), here("data", "splits", data_id, "holdout_100000"))


  train_splits = lapply(sizes, function(size) {
    set.seed(data_id + size)
    print(size)
    task$row_roles$use = sample(partition_ids, size * n_subsets)
    resampling = rsmp("cv", folds = n_subsets)$instantiate(task)
    inst = resampling$instance
    names(inst) = c("row_id", "iter")
    inst = inst[, c("iter", "row_id")]

    write_parquet(inst, here("data", "splits", data_id, paste0(size)))
  })

  train_splits = set_names(train_splits, as.character(sizes))

  NULL
})
