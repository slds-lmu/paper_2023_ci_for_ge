library(data.table)
library(mlr3)
library(mlr3db)
library(mlr3batchmark)
library(batchtools)


# This is where all the datasets lay
DATA_PATH = "TODO"

# we always apply all learners







make_tasks = function(task_id, target, features, dataset_size) {
  backend = as_duckdb_backend(file.path(path, paste0(task_id, ".pq")))
  task = as_task_classif(backend = backend, target = target)
  task$set_col_roles(features, "feature")
  task$set_row_roles(5000001:5100000, "holdout")
  task$id = task_id

  task_list = list()

  for (i in seq_len(1000)) {
    task_new = task$clone(deep = TRUE)
    task_new$set_row_roles((dataset_size * (i - 1) + 1):(i * dataset_size))
    task_list[[i]] = task_new
  }
  task_list
}

# this should return a list with: "task_type", "nrow", "ncol", "features", "target"
load_task_info = function(task_id) {
  jsonlite::read_json(here::here("data", "task_info", paste0(task_id, ".json")))
}

# We start by creating a data.table
# task_id | learner_id | resampling_id | dataset_size
# ...     | ...        | ...           | ...
# * This table must already take into account which resamplings we want to apply for which dataset sizes
# * The learner_id is not task-dependent here (corresponding learner is created in make_learner)
# * Each row is repreated 1000 times

make_design = function() {

}

make_replications = function(task_id, learner_id, resampling_id, dataset_size) {



  task_info = load_task_info(task_id)

  task_type = task_info$task_type
  features = task_info$features
  target = task_info$target

  learner = make_learner(learner_id, task_type)
  # TODO: Check again that this really does what we want
  learner$predict_sets = c("test", "holdout")
  resampling = make_resampling(resampling_id)

  tasks = make_tasks(
    task_id = task_id,
    path = DATA_PATH,
    target = target,
    features = features,
    dataset_size = dataset_size
  )


  design = benchmark_grid(
    tasks = tasks,
    learners = learner,
    resampling = resampling
  )

  return(design)
}

for (i in seq_len(nrow(design))) {
  learner_id = design[i, "learner"][[1]][1]
  task_id = design[i, "learner"][[1]][1]
  resampling_id = design[i, "learner"][[1]][1]
  dataset_size = design[i, "dataset_size"][[1L]][1L]

  design = make_replications(
    learner_id = learner_id,
    task_id = task_id,
    resampling_id = resampling_id,
    dataset_size = dataset_size
  )

  # TODO: Probably I want to save the designs somewhere to test them
  batchmark(design)
}

