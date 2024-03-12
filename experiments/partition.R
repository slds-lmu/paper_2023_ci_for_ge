library(mlr3oml)
library(mlr3misc)
library(mlr3)
options(mlr3oml.parquet = TRUE)
options(mlr3oml.cache = TRUE)
n_data = 5100000L
n_subsets = 500L
sizes = c(50L, 100L, 500L, 1000L, 5000L, 10000L)

data_ids = unname(unlist(list(
  classif = c(45570, 45689, 45704, 45654, 45665, 45668, 45669, 45672, 45693),
  regr = c(45692, 45694, 45655, 45666, 45667, 45670, 45671, 45695, 45696)
)))

#walk(data_ids, function(id) odt(id)$data)

future::plan("multicore", workers = 5)

splits = lapply(data_ids, function(data_id) {
  print(data_id)
  if (!dir.exists(paste0("~/gh/paper_2023_ci_for_ge/data/splits/", data_id))) {
    dir.create(paste0("~/gh/paper_2023_ci_for_ge/data/splits/", data_id), recursive = TRUE)
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
  write.csv(holdout_ids[1:50000], paste0("~/gh/paper_2023_ci_for_ge/data/splits/", data_id, "/",  "holdout_50000.csv"), row.names = FALSE)
  write.csv(holdout_ids, paste0("~/gh/paper_2023_ci_for_ge/data/splits/", data_id, "/",  "holdout_100000.csv"), row.names = FALSE)


  train_splits = lapply(sizes, function(size) {
    print(size)
    task$row_roles$use = sample(partition_ids, size * n_subsets)
    resampling = rsmp("cv", folds = n_subsets)$instantiate(task)
    inst = resampling$instance
    names(inst) = c("row_id", "iter")
    inst = inst[, c("iter", "row_id")]

    write.csv(inst, paste0("~/gh/paper_2023_ci_for_ge/data/splits/", data_id, "/",  size, ".csv"), row.names = FALSE)
  })

  train_splits = set_names(train_splits, as.character(sizes))

  NULL
  #list(
  #  holdout = holdout_ids,
  #  train = train_splits
  #)
})
