library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3db)
library(mlr3oml)
library(mlr3misc)
library(mlr3batchmark)
library(batchtools)

TEST = TRUE

if (is.null(getOption("mlr3oml.cache")) || isFALSE(getOption("mlr3oml.cache"))) {
  stop("Pleasure configure the option mlr3oml.cache to TRUE or a specific path.")
}

if (TEST) {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/test3"
} else {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

SEED = 42

if (!file.exists(REGISTRY_PATH)) {
  makeExperimentRegistry(
    file.dir = REGISTRY_PATH,
    seed = SEED,
    packages = c("mlr3", "mlr3learners", "mlr3pipelines"),
    work.dir = here::here("experiments")
  )
} else {
  reg = loadRegistry(REGISTRY_PATH, writeable = TEST)
}


# here we have two sublists:
# * other: resamplings are applies to both the small and large setting
# * small: resamplings that are only applied in the small setting
#
# TODO: We need to check that all methods are covered by that:
# There are more methods than entries here (multiple methods for CV e.g.)
# For every resampling we do not only make predictions on the test set, bu also on the holdout set
if (TEST) {
  resampling_ids = list(other = list(
    holdout           = list(id = "holdout", params = list())
  ), small = list(
    loo               = list(id = "loo", params = list())
  ))
} else {
  #resampling_ids = list(other = list(
  #  #holdout is the same as subsampling with 1 repetition
  #  holdout           = list(id = "holdout", params = list()),
  #  subsampling_10    = list(id = "subsampling", params = list(repeats = 10)),
  #  subsampling_50    = list(id = "subsampling", params = list(repeats = 30)),
  #  subsampling_100   = list(id = "subsampling", params = list(repeats = 30)),
  #  cv_10             = list(id = "cv", params = list(folds = 10)),
  #  repeated_cv_5_10  = list(id = "repeated_cv", params = list()),
  #  repeated_cv_10_10 = list(id = "repeated_cv", params = list()),
  #  nested_cv         = list(id = "nested_cv", params = list("TODO")),
  #  conervative_z     = list(id = "conservative_z", params = list("TODO"))
  #  diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
  #  # gives the true prediction error
  #  prediction_error  = list(id = "holdout", params = list(ratio = 1))
  #  ), small = list(
  #  loo               = list(id = "loo", params = list())
  #  )
  #)
}

if (TEST) {
  learner_ids = list(regr = list(
    linear = list(id = "regr.lm", params = list())
    ), classif = list(
    linear = list(id = "classif.log_reg", params = list())
    )
  )
} else {
  learner_ids = list(regr = list(
    linear = list(id = "classif.log_reg", params = list()),
    ranger10 = list(id = "classif.ranger", params = list(num.trees = 10)),
    ranger100 = list(id = "classif.ranger", params = list(num.trees = 100))
    ), classif = list(
    linear = list(id = "regr.lm", params = list()),
    ranger10 = list(id = "regr.ranger", params = list(num.trees = 10)),
    ranger100 = list(id = "regr.ranger", params = list(num.trees = 100)))
  )
}


# @param x An element from the learner_ids list.
# @param name The name from the learner_ids list.
# @param task The task for which to create the learner (classif or regr).

make_learner = function(x) {
  learner = do.call(lrn, 
    args = c(list(.key = x$id), x$params)
  )
  if (startsWith(x$id, "classif")) {
    learner$predict_type = "prob"
  }
  graph = ppl("robustify", learner = learner) %>>% learner
  learner = as_learner(graph)
  learner$id = x$id
  return(learner)
}

learners = map(learner_ids, function(xs) {
  map(xs, function(x) {
    make_learner(x)
  })
})

if (TEST) {
  task_ids = list(classif = list(
    list(name = "bates_classif_20", id = NA, target = "y", task_type = "classif")),
    regr = list(
    list(name = "bates_regr_20", id = 45655, target = "y", task_type = "regr"))
  )
} else {
  # TODO: Update IDs
  # Parquet files are currently broken on OpenML
  task_ids = list(classif = list(
    list(name = "adult", id = NA, target = "class", task_type = "classif"),
    list(name = "electricity", id = NA, target = "class", task_type = "classif"),
    list(name = "bank_marketing", id = NA, target = "Class", task_type = "classif"),
    list(name = "covertype", id = NA, target = "Y", task_type = "classif"),

    # simplisically simulated
    list(name = "bates_classif_100", id = NA, target = "y", task_type = "classif"),
    list(name = "bates_classif_20", id = NA, target = "y", task_type = "classif"),

    # "real" (simulated with physical simulator)
    list(name = "higgs", id = NA, target = "Target", task_type = "classif"),

    # simulated with covariance matrix
    list(name = "prostate", id = NA, target = "y", task_type = "classif"),
    list(name = "colon", id = NA, "y", task_type = "classif"),
    list(name = "breast", id = NA, "y", task_type = "classif")),

    # -----------
    # regression:
    # -----------

    # simulated with LLM
    regr = list(
    list(name = "diamonds", id = NA, target = "price", task_type = "regr"),
    list(name = "sgemm_gpu_kernel_performance", id = NA, target = "Run1", task_type = "regr"),
    list(name = "physiochemical_protein", id = NA, target = "RMSD", task_type = "regr"),
    list(name = "video_transcoding", id = NA, target = "utime", task_type = "regr"),
    # FIXME
    # video_transcoding = "TODO",

    # simplistically simulated
    list(name = "bates_regr_100", id = NA, target = "y", task_type = "regr"),
    list(name = "bates_regr_20", id = NA, target = "y", task_type = "regr"),
    list(name = "chen_10", id = NA, target = "y", task_type = "regr"))
  )
}


dataset_sizes = list(
  small = c(50, 100, 200),
  other = c(500, 1000, 2000, 5000)
)

make_task = function(x) {
}

make_tasks = function(x, sizes, n_replications) {
  path = file.path("/gscratch", "sfische6", "ci_for_ge_data", paste0(x$name, ".parquet"))

  make_connector = function(data) {
    force(data)
    function() {
      checkmate::assert_file_exists(data, access = "r", extension = "parquet")
      con = DBI::dbConnect(duckdb::duckdb(), read_only = TRUE)

      query = "CREATE OR REPLACE VIEW 'mlr3db_view' AS SELECT *"
      primary_key = "mlr3_row_id"
      query = paste0(query, ", row_number() OVER () AS mlr3_row_id")

      query = sprintf("%s FROM parquet_scan(['%s'])", query, paste0(data, collapse = "','"))
      DBI::dbExecute(con, query)

      con
    }
  }


  holdout_ids = 5000001:5100000

  make_use = function(size, replication) {
    seq(1, size) + size * (rep - 1)
  }
  
  connector = make_connector(path)

  make_backend = function(size, replication) {
    use_rows = make_use(size, replication)

    all_rows = c(use_rows, holdout_ids)

    backend = as_duckdb_backend(path)
    backend$connector = connector


  }

  backend = as_duckdb_backend(path)
  backend$connector = make_connector(path)

  column_info = mlr3:::col_info(backend)


  # all datasets contain only feature and target so we don't have to set the features

  converter = if (x$task_type == "regr") as_task_regr else as_task_classif
  ids = 

  # Here we cou
  task$row_roles$holdout = 5000001:5100000

  tasks = map(sizes, function(size) {
    map(seq_len(n_replications), function(rep) {
      backend = make_backend
      task_copy$row_roles$use = seq(1, size) + size * (rep - 1)
      task_copy$id = paste(x$name, "size", rep, sep = "_")
      task_copy
    })
  })
}

# TODO: Change make_task if parquet data is available from OpenML

# We start by creating a data.table
# task_id | learner_id | resampling_id | dataset_size
# ...     | ...        | ...           | ...
# * This table must already take into account which resamplings we want to apply for which dataset sizes
# * The learner_id is not task-dependent here (corresponding learner is created in make_learner)
# * Each row is repreated 1000 times

resamplings = map(c("small", "other"), function(category) {
  imap(resampling_ids[[category]], function(x, name) {
    do.call(rsmp, args = c(list(.key = x$id), x$params))
  })
})
names(resamplings) = c("small", "other")

if (TEST) {
  n_replications = 2
} else {
  n_replications = 1000
}

tasks = map(task_ids, function(xs) {
  # xs is task_ids$regr or task_ids$classif
  task_list = map(c("small", "other"), function(category) {
    res = map(xs, function(x) {
      make_tasks(x, sizes = dataset_sizes[[category]], n_replications = n_replications)
    })
    unlist(res, recursive = TRUE)
  })
  names(task_list) = c("small", "other")
  task_list
})

design_small_regr = benchmark_grid(
  tasks = tasks$regr$small,
  learners = learners$regr,
  resamplings = resamplings$small
)
batchmark(design_small_regr)


#design_other_regr = benchmark_grid(
#  tasks = tasks$regr$other,
#  learners = learners$regr,
#  resamplings = resamplings$other
#)
#
#design_small_classif = benchmark_grid(
#  tasks = tasks$classif$small,
#  learners = learners$classif,
#  resamplings = resamplings$small
#)
#
#design_other_classif = benchmark_grid(
#  tasks = tasks$classif$other,
#  learners = learners$classif,
#  resamplings = resamplings$other
#)
#
#design = rbindlist(list(
#  design_other_regr,
#  design_small_regr,
#  design_other_classif,
#  design_small_classif
#))
#
#
#batchmark(design)
#