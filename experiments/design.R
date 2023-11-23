library(batchtools)
library(mlr3misc)
library(mlr3oml)
devtools::load_all("/pfs/tc1/home/sfische6/paper_2023_ci_for_ge/inferGE")

if (is.null(getOption("mlr3oml.cache")) || isFALSE(getOption("mlr3oml.cache"))) {
  stop("Pleasure configure the option mlr3oml.cache to TRUE or a specific path.")
}

data_ids = list(
  # FIXME:  add 45668 again (regr)
  classif = c(45654, 45665, 45669, 45672, 45689, 45703, 45704, 45693, 45668),
  regr = c(45664, 45655, 45666, 45670, 45671, 45692, 45694, 45695, 45696)
)

SEED = 42
TEST = TRUE

REGISTRY_PATH = if (TEST) { # nolint
  "/gscratch/sfische6/benchmarks/ci_for_ge/newtest22"
} else {
  "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

if (!file.exists(REGISTRY_PATH)) {
  makeExperimentRegistry(
    file.dir = REGISTRY_PATH,
    seed = SEED,
    packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "torch", "mlr3misc"),
    work.dir = here::here("experiments")
  )
} else {
  reg = loadRegistry(REGISTRY_PATH, writeable = TEST)
}

N_REP = if (TEST) { # nolint
  1L
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
    cv_5             = list(id = "cv", params = list(folds = 10)),
    cv_10             = list(id = "cv", params = list(folds = 10)),
    cv_20             = list(id = "cv", params = list(folds = 10)),
    #repeated_cv_10_10 = list(id = "repeated_cv", params = list(folds = 10, repeats = 10)),
    conservative_z    = list(id = "conservative_z", params = list(J = 10, M = 10, ratio = 0.9)),
    diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
    prediction_error  = list(id = "holdout", params = list(ratio = 1)),
    bootstrap_10      = list(id = "bootstrap", params = list(ratio = 1, repeats = 10)),
    bootstrap_100     = list(id = "bootstrap", params = list(ratio = 1, repeats = 100)),
    # needed for the bootstrap method
    insample          = list(id = "insample", params = list())
  ), small = list(
    loo               = list(id = "loo", params = list()),
    austern_zhou      = list(id = "austern_zhou", params = list(folds = 10)),
    bootstrap_ccv     = list(id = "bootstrap_ccv", params = list(ratio = 1, repeats = 10))
  ))
} else {
  stop("not done yet")
}

SIZES = if (TEST) {
  list(
    small = c(50, 100, 200),
    other = c(500, 1000, 2000, 5000)
  )
} else {
  stop("not done yet")
}

TASKS = if (TEST) {
  data_ids
} else {
  stop("not done yet")
}


LEARNERS = if (TEST) {
  make_learner_list = function(task_type) {
    f = function(x) {
      paste0(task_type, ".", x)
    }
    list(
      ridge001 = list(id = f("glmnet"), params = list(alpha = 0, lambda = 0.01)),
      ridge005  = list(id = f("glmnet"), params = list(alpha = 0, lambda = 0.05)),
      rpart1  = list(id = f("ranger"), params = list(num.trees = 1)),
      ranger100 = list(id = f("ranger"), params = list(num.trees = 100)),
      tabnet001 = list(id = f("tabnet"), params = list(momentum = 0.01)),
      tabnet005 = list(id = f("tabnet"), params = list(momentum = 0.05))
    )
  }

  LEARNERS = list(
    regr = make_learner_list("regr"),
    classif =  make_learner_list("classif")
  )
} else {
  stop("not done yet")
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

make_learner = function(learner_id, learner_params, task, resampling) {
  learner = do.call(lrn,
    args = c(list(.key = learner_id), learner_params)
  )


  # we need this because bootstrapping is broken with mlr3pipelines
  graph = ppl("robustify", learner = learner, task = task) %>>%
    learner

  if (startsWith(class(resampling)[[1L]], "ResamplingBootstrap")) {
    graph = inferGE::PipeOpMetaRobustify$new() %>>% graph
  }

  learner = as_learner(graph)

  if (startsWith(learner_id, "classif")) {
    fallback = lrn("classif.featureless")
    fallback$predict_type = learner$predict_type = "prob"
  } else {
    fallback = lrn("regr.featureless")
  }
  learner$predict_sets = c("test", "holdout")
  learner$encapsulate = c(train = "try", predict = "try")
  learner$fallback = fallback
  learner$id = learner_id
  return(learner)
}

make_task = function(data_id, size, repl) {
  ids_use = seq(1, size) + size * (repl - 1)
  ids_holdout = 5000001:5100000
  ids = c(ids_use, ids_holdout)

  odata = odt(data_id, parquet = TRUE)
  target = odata$target_names

  backend = as_data_backend(odata)

  tmpdata = backend$data(ids, backend$colnames)
  # mlr3 bugs in cbind ...
  names(tmpdata)[names(tmpdata) == "mlr3_row_id"] = "..row_id"
  
  in_memory_backend = as_data_backend(tmpdata, primary_key = "..row_id")

  task = if (is.factor(tmpdata[[target]])) {
    as_task_classif(in_memory_backend, target = target)
  } else {
    as_task_regr(in_memory_backend, target = target)
  }

  task$id = odata$name

  task$row_roles$use = ids_use
  task$row_roles$holdout = ids_holdout

  return(task)
}

make_resampling = function(resampling_id, resampling_params) {
  resampling = do.call(rsmp, c(list(.key = resampling_id), resampling_params[[1]]))
}

run_resampling = function(instance, resampling_id, resampling_params, job, ...) {
  task = make_task(data_id = instance$data_id, size = instance$size, repl = job$repl)
  resampling = make_resampling(resampling_id, resampling_params)
  # we pass the task to make_learner() so we can skip some robustify steps
  # we pass the resampling to make_learner to know when we need the metarobustify-step (for bootstrap)
  learner = make_learner(instance$learner_id, instance$learner_params[[1]], task = task, resampling = resampling)
  resampling$instantiate(task)

  if (!("factor" %in% learner$properties) && ("factor" %in% task$feature_types$type)) {
    task = po("encode")$train(list(task))[[1L]]
  }

  # FIXME: Maybe we want better seeding here  so that the resampling splits are the same across
  # different learners on the same task. but probably not that important
  rr = resample(task, learner, resampling, store_models = FALSE, store_backends = FALSE)

  if (task$task_type == "regr") {
    measures = msrs(paste0("regr.", c("rmse", "mae")))
  } else if (task$task_type == "classif") {
    measures = msrs(paste0("classif.", c("acc", "bacc", "precision", "recall", "sensitivity", "specificity")))
  }

  # FIXME: Is this output enough to calculate all the proxy quantities?
  list(
    test_predictions = map(rr$predictions("test"), data.table::as.data.table),
    holdout_predictions = map(rr$predictions("holdout"), function(x) x$score(measures)),
    #holdout_scores = rr$score(measures, predict_sets = "holdout"),
    resampling = resampling
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
  function(learner_id, learner_params, data_id, target, size, ...) {
    list(
      learner_id = learner_id,
      learner_params = learner_params,
      data_id = data_id,
      size = size
    )
  },
  seed = 314
)


# type is either "other" or "small"
make_prob_designs = function(type) {
  prob_designs_other = map(c("regr", "classif"), function(task_type) {
    dt = CJ(
      data_id = TASKS[[task_type]],
      learner = LEARNERS[[task_type]],
      size = SIZES[[type]],
      sorted = FALSE
    )

    new = data.table()

    new$data_id = dt$data_id
    new$size = dt$size
    new$learner_id = map_chr(dt$learner, "id")
    new$learner_params = map(dt$learner, function(x) list(x$params))
    new$task_type = task_type

    new
  }) |> rbindlist()
}

prob_design_small = make_prob_designs("small")
prob_design_other = make_prob_designs("other")

# Add experiments for "other" resamplings

# type is either "other" or "small"
make_algo_design = function(type) {
  algo_designs_other = data.table(
    resampling_id = map_chr(RESAMPLINGS[[type]], "id"),
    resampling_params = map(RESAMPLINGS[[type]], function(x) list(x$params)),
    # The resampling name is just so we can easier identify the resamplings later
    resampling_name = names(RESAMPLINGS[[type]])
  )
}

# resampling methods LOO, BCCV, Austern & Zhoy
algo_design_small = make_algo_design("small")
# All other resampling methods
algo_design_other = make_algo_design("other")


# Applying all small resampling methods to small problems
addExperiments(
  algo.designs = list(run_resampling = algo_design_small),
  prob.designs = list(ci_estimation = prob_design_small),
  repls = N_REP
)
# Applying all other resampling methods to small problems
addExperiments(
  algo.designs = list(run_resampling = algo_design_other),
  prob.designs = list(ci_estimation = prob_design_small),
  repls = N_REP
)

# Apply other resampling methods to other problems

addExperiments(
  algo.designs = list(run_resampling = algo_design_other),
  prob.designs = list(ci_estimation = prob_design_other),
  repls = N_REP
)


jt = getJobTable() |> unwrap()

ids = jt$job.id
chunks = data.table(
  job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 100, shuffle = FALSE)
)

if (FALSE) {
  testJob(1)
  testJob(10794) # tabnet and classif
  # test bootstrap
}
#submitJobs(chunks)
