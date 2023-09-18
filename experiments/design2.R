library(batchtools)
library(mlr3misc)
devtools::load_all("/pfs/tc1/home/sfische6/paper_2023_ci_for_ge/inferGE")

if (is.null(getOption("mlr3oml.cache")) || isFALSE(getOption("mlr3oml.cache"))) {
  stop("Pleasure configure the option mlr3oml.cache to TRUE or a specific path.")
}

SEED = 42
TEST = TRUE

REGISTRY_PATH = if (TEST) { # nolint
  "/gscratch/sfische6/benchmarks/ci_for_ge/newtest2"
} else {
  "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

if (!file.exists(REGISTRY_PATH)) {
  makeExperimentRegistry(
    file.dir = REGISTRY_PATH,
    seed = SEED,
    packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE"),
    work.dir = here::here("experiments")
  )
} else {
  reg = loadRegistry(REGISTRY_PATH, writeable = TEST)
}

N_REP = if (TEST) { # nolint
  1000L
} else {
  1000L
}
# FIXME: Also include parameters where we use the default (infer_xxx needs the parameter values)
RESAMPLINGS = if (TEST) {
  list(other = list(
    holdout           = list(id = "holdout", params = list(ratio = 2 / 3)),
    nested_cv         = list(id = "nested_cv", params = list(folds = 10)),
    subsampling_10    = list(id = "subsampling", params = list(repeats = 10)),
    subsampling_100   = list(id = "subsampling", params = list(repeats = 100)),
    cv_10             = list(id = "cv", params = list(folds = 10)),
    repeated_cv_10_10 = list(id = "repeated_cv", params = list(folds = 10, repeats = 10)),
    conservative_z    = list(id = "conservative_z", params = list(J = 10, M = 10, ratio = 0.9)),
    diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
    prediction_error  = list(id = "holdout", params = list(ratio = 1))
  ), small = list(
    loo               = list(id = "loo", params = list())
  ))
} else {
  list(other = list(
    #holdout is the same as subsampling with 1 repetition
    holdout           = list(id = "holdout", params = list()),
    subsampling_10    = list(id = "subsampling", params = list(repeats = 10)),
    subsampling_50    = list(id = "subsampling", params = list(repeats = 30)),
    subsampling_100   = list(id = "subsampling", params = list(repeats = 30)),
    cv_10             = list(id = "cv", params = list(folds = 10)),
    repeated_cv_5_10  = list(id = "repeated_cv", params = list()),
    repeated_cv_10_10 = list(id = "repeated_cv", params = list()),
    nested_cv         = list(id = "nested_cv", params = list("TODO")),
    conervative_z     = list(id = "conservative_z", params = list("TODO")),
    diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
    # gives the true prediction error
    ), small = list(
    loo               = list(id = "loo", params = list())
    )
  )
}

SIZES = if (TEST) {
  list(
    small = c(50, 200),
    other = c(500, 1000)
  )
} else {
  list(
    small = c(50, 100, 200),
    other = c(500, 1000, 2000, 5000)
  )
}

TASKS = if (TEST) {
  list(classif = list(
    list(id = "simulated_electricity", target = "class")),
    regr = list(
    list(id = "simulated_physiochemical_protein", target = "RMSD"))
  )
} else {
  list(classif = list(
    list(id = "simulated_adult", id = NA, target = "class", task_type = "classif"),
    list(id = "simulated_electricity", id = NA, target = "class", task_type = "classif"),
    list(id = "simulated_bank_marketing", id = NA, target = "Class", task_type = "classif"),
    list(id = "simulated_covertype", id = NA, target = "Y", task_type = "classif"),

    # simplisically simulated
    list(id = "bates_classif_100", id = NA, target = "y", task_type = "classif"),
    list(id = "bates_classif_20", id = NA, target = "y", task_type = "classif"),

    # "real" (simulated with physical simulator)
    list(id = "higgs", id = NA, target = "Target", task_type = "classif"),

    # simulated with covariance matrix
    list(id = "prostate", id = NA, target = "y", task_type = "classif"),
    list(id = "colon", id = NA, "y", task_type = "classif"),
    list(id = "breast", id = NA, "y", task_type = "classif")),

    # -----------
    # regression:
    # -----------

    # simulated with LLM
    regr = list(
    list(id = "simulated_diamonds", id = NA, target = "price", task_type = "regr"),
    list(id = "simulated_sgemm_gpu_kernel_performance", id = NA, target = "Run1", task_type = "regr"),
    list(id = "simulated_physiochemical_protein", id = NA, target = "RMSD", task_type = "regr"),
    list(id = "simulated_video_transcoding", id = NA, target = "utime", task_type = "regr"),

    # simplistically simulated
    list(id = "bates_regr_100", id = NA, target = "y", task_type = "regr"),
    list(id = "bates_regr_20", id = NA, target = "y", task_type = "regr"),
    list(id = "chen_10", id = NA, target = "y", task_type = "regr")))
}

LEARNERS = if (TEST) {
  list(regr = list(
    linear = list(id = "regr.lm", params = list())
    ), classif = list(
    linear = list(id = "classif.log_reg", params = list())
    )
  )
} else {
  list(regr = list(
    linear = list(id = "classif.log_reg", params = list()),
    ranger10 = list(id = "classif.ranger", params = list(num.trees = 10)),
    ranger100 = list(id = "classif.ranger", params = list(num.trees = 100))
    ), classif = list(
    linear = list(id = "regr.lm", params = list()),
    ranger10 = list(id = "regr.ranger", params = list(num.trees = 10)),
    ranger100 = list(id = "regr.ranger", params = list(num.trees = 100)))
  )
}


# here we have two sublists:
# * other: resamplings are applies to both the small and large setting
# * small: resamplings that are only applied in the small setting
#
# TODO: We need to check that all methods are covered by that:
# There are more methods than entries here (multiple methods for CV e.g.)
# For every resampling we do not only make predictions on the test set, bu also on the holdout set


# @param x An element from the learner_ids list.
# @param name The name from the learner_ids list.
# @param task The task for which to create the learner (classif or regr).

make_learner = function(learner_id, learner_params) {
  learner = do.call(lrn,
    args = c(list(.key = learner_id), learner_params)
  )
  if (startsWith(learner_id, "classif")) {
    fallback = lrn("classif.featureless")
    fallback$predict_type = learner$predict_type = "prob"
  } else {
    fallback = lrn("regr.featureless")
  }
  graph = ppl("robustify", learner = learner) %>>% learner
  learner = as_learner(graph)
  learner$predict_sets = c("test", "holdout")
  learner$encapsulate = c(train = "try", predict = "try")
  learner$fallback = fallback
  learner$id = learner_id
  return(learner)
}

make_task = function(task_id, target, size, repl) {
  # FIXME: replace this with OpenML once the parquet files are available
  path = file.path("/gscratch", "sfische6", "ci_for_ge_data", paste0(task_id, ".parquet"))

  ids_use = seq(1, size) + size * (repl - 1)
  ids_holdout = 5000001:5100000
  ids = c(ids_use, ids_holdout)

  backend = as_duckdb_backend(path)

  data = backend$data(ids, backend$colnames)

  in_memory_backend = as_data_backend(data, primary_key = backend$primary_key)

  is_classif = is.factor(in_memory_backend$head(1)[[target]])

  task = if (is_classif) {
    as_task_classif(in_memory_backend, target = target)
  } else {
    as_task_regr(in_memory_backend, target = target)
  }

  task$id = task_id

  task$row_roles$use = ids_use
  task$row_roles$holdout = ids_holdout

  return(task)
}

make_resampling = function(resampling_id, resampling_params) {
  resampling = do.call(rsmp, c(list(.key = resampling_id), resampling_params))
}

run_resampling = function(instance, resampling_id, resampling_params, job, ...) {
  learner = make_learner(instance$learner_id, instance$learner_params)
  task = make_task(task_id = instance$task_id, target = instance$target, size = instance$size, repl = job$repl)
  resampling = make_resampling(resampling_id, resampling_params)
  #return(list(
  #  learner = learner,
  #  task = task,
  #  resampling = resampling
  #))

  # FIXME: Maybe we want better seeding here  so that the resampling splits are the same across
  # different learners on the same task. but probably not that important
  rr = resample(task, learner, resampling, store_models = FALSE)

  if (task$task_type == "regr") {
    measures = msrs(paste0("regr.", c("rmse", "mae")))
  } else if (task$task_type == "classif") {
    measures = msrs(paste0("classif.", c("acc", "bacc", "precision", "recall", "sensity", "specificity")))
  }

  predictions = rr$predictions("")

  list(
    test_predictions = rr$predictions("test"),
    holdout_scores = rr$score(measures, predict_sets = "holdout")
  )
}

batchExport(list(
  make_task = make_task,
  make_learner = make_learner,
  make_resampling = make_resampling
))

addAlgorithm(
  "run_resampling",
  fun = run_resampling
)

addProblem(
  "ci_estimation",
  data = NULL,
  function(learner_id, learner_params, task_id, target, size, ...) {
    list(
      learner_id = learner_id,
      learner_params = learner_params,
      task_id = task_id,
      target = target,
      size = size
    )
  },
  seed = 314
)


# Add experiments for "other" resamplings
prob_designs_other = map(c("regr", "classif"), function(task_type) {
  dt = CJ(
    task = TASKS[[task_type]],
    learner = LEARNERS[[task_type]],
    size = SIZES$other,
    sorted = FALSE
  )

  new = data.table()

  new$task_id = map_chr(dt$task, "id")
  new$target = map_chr(dt$task, "target")
  new$size = dt$size
  new$learner_id = map_chr(dt$learner, "id")
  new$learner_params = map(dt$learner, "params")

  new
}) |> rbindlist()


algo_designs_other = data.table(
  resampling_id = map_chr(RESAMPLINGS$other, "id"),
  resampling_params = map(RESAMPLINGS$other, "params"),
  # The resampling name is just so we can easier identify the resamplings later
  resampling_name = names(RESAMPLINGS$other)
)

addExperiments(
  prob.designs = list(ci_estimation = prob_designs_other),
  algo.designs = list(run_resampling = algo_designs_other),
  repls = N_REP
)

job_table = getJobTable(findExpired())

ids = job_table$job.id
chunks = data.table(
  job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 100, shuffle = FALSE)
)

submitJobs(chunks)