library(data.table)
library(mlr3)
library(mlr3db)
library(mlr3batchmark)
library(batchtools)


# This is where all the datasets lie
DATA_PATH = "TODO"

# the ids here are generic, i.e. they will be translated to clasisf and regr accordingly
learner_ids = list(
  "ranger_10",
  "ranger_100",
  "linear",
  "linear_penalized"
)

# we always apply all learners

task_ids = list(classif = list(
  # simulated with LLM
  "adult",
  "electricity",
  "bank_marketing",

  # simplisically simulated
  "bates_classif_100",
  "bates_classif_20",

  # "real" (simulated with physical simulator)
  "higgs",

  # simulated with covariance matrix
  "prostate",
  "colon",
  "breast",
  ), regr = list(
  # simulated with LLM
  "diamonds",
  "sgemm_gpu_kernel_performance",
  "physiochemical_protein",
  "covertype",

  # simplistically simulated
  "bates_regr_100",
  "bates_regr_20",
  "chen_10",
  )
)

# here we have two sublists:
# * both: resamplings are applies to both the small and large setting
# * small: resamplings that are only applied to the small dataset sizes
#
# TODO: We need to check that all methods are covered by that:
# There are more methods than entries here (multiple methods for CV e.g.)
resampling_ids = list(both = list(
  holdout           = list(id = "holdout", pars = list()),
  subsampling_10    = list(id = "subsampling", pars = list(repeats = 10)),
  subsampling_30    = list(id = "subsampling", pars = list(repeats = 30)),
  cv_10             = list(id = "cv", pars = list()),
  repeated_cv_10_10 = list(id = "repeated_cv", pars = list()),
  nested_cv         = list(id = "nested_cv", pars = list("TODO")),
  nadeau            = list("TODO"), # They use subsampling I believe
  diettrich         = list(id = "repeated_cv", pars = list(repeats = 5, folds = 2))
  )


  "nested_cv", # 10 x 10
  "loo", # only for small datasets
  "boostrap", # do we even want this?
  "jiang", # only for very small datasets (also does loo)
)




make_learner = function(id, task_type) {
  if (task_type == "regr") {
    learner = switch(id,
      linear =

      stop("Unknown learner id")
    )
  } else if (task_type == "classif") {
    learner = switch(id,
      linear = lrn("")
      stop("Unknown learner id")
    )

    if ("prob" %in% learner$predict_types) {
      learner$predict_type = "prob"
    }
  } else {
    stop("Unsupported task type")
  }

  graph = po("")


}

# takes in a resampling id and creates the resamplin
make_resampling = function(id) {
  switch(id,
    stop("Unknown resampling.s")
  )
}

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

