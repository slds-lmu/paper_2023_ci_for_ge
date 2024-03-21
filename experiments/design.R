library(batchtools)
library(mlr3misc)
library(mlr3oml)
library(duckdb)
devtools::load_all("/pfs/tc1/home/sfische6/paper_2023_ci_for_ge/inferGE")

source(here::here("experiments", "helper.R"))

if (is.null(getOption("mlr3oml.cache")) || isFALSE(getOption("mlr3oml.cache"))) {
  stop("Pleasure configure the option mlr3oml.cache to TRUE or a specific path.")
}

data_ids = list(
  classif = c(45570, 45689, 45704, 45654, 45665, 45668, 45669, 45672, 45693),
  regr = c(45692, 45694, 45655, 45666, 45667, 45670, 45671, 45695, 45696)
)

data_names = list(
  "45570" = "higgs",
  "45689" = "adult",
  "45704" = "covertype",
  "45654" = "bates_classif_20",
  "45665" = "colon",
  "45668" = "bates_classif_100",
  "45669" = "breast",
  "45672" = "prostate",
  "45693" = "electricity",
  "45692" = "diamonds",
  "45694" = "phyisiochemical_protein",
  "45655" = "bates_regr_20",
  "45666" = "friedman1",
  "45667" = "bates_regr_100",
  "45670" = "chen_10_null",
  "45671" = "chen_10",
  "45695" = "sgemm_gpu",
  "45696" = "video_transcoding"
)

SEED = 42
TEST = TRUE

REGISTRY_PATH = if (TEST) { # nolint
  "/gscratch/sfische6/benchmarks/ci_for_ge/run_big"
} else {
  "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

if (!file.exists(REGISTRY_PATH)) {
  reg = makeExperimentRegistry(
    file.dir = REGISTRY_PATH,
    seed = SEED,
    packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr"),
    work.dir = here::here()
  )
} else {
  reg = loadRegistry(REGISTRY_PATH, writeable = TEST)
}

N_REP = if (TEST) { # nolint
  1L
} else {
  500L
}
# FIXME: Also include parameters where we use the default (infer_xxx needs the parameter values)
RESAMPLINGS = if (TEST) {
  list(other = list(
    holdout            = list(id = "holdout",          params = list(ratio = 2 / 3)),
    subsampling_10     = list(id = "subsampling",      params = list(repeats = 10, ratio = 0.9)),
    subsampling_50     = list(id = "subsampling",      params = list(repeats = 50, ratio = 0.9)),
    cv_10              = list(id = "cv",               params = list(folds = 10)),
    diettrich          = list(id = "repeated_cv",      params = list(repeats = 5, folds = 2)),
    bootstrap_50       = list(id = "bootstrap",        params = list(ratio = 1, repeats = 50)),
    bootstrap_100      = list(id = "bootstrap",        params = list(ratio = 1, repeats = 100)),
    insample           = list(id = "insample",         params = list())
  ), small = list(
    nested_cv          = list(id = "nested_cv",        params = list(folds = 5, repeats = 200)),
    conservative_z     = list(id = "conservative_z",   params = list(J = 15, M = 10, ratio = 0.9)),
    two_stage          = list(id = "nested_bootstrap", params = list(reps_outer = 200, reps_inner = 10)),
    loo                = list(id = "loo",              params = list()),
    austern_zhou       = list(id = "austern_zhou",     params = list(folds = 5, repeats = 1)),
    austern_zhou_rep   = list(id = "austern_zhou",     params = list(folds = 5, repeats = 5)),
    bootstrap_ccv      = list(id = "bootstrap_ccv",    params = list(ratio = 1, repeats = 100))
  ))
} else {
  stop("not done yet")
}

SIZES = if (TEST) {
  list(
    small = c(100L, 500L),
    other = c(1000L, 5000L, 10000L)
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
    learners = list(
      ridge  = list(id = f("cv_glmnet"), params = list(alpha = 0, nfolds = 3L)),
      rpart  = list(id = f("rpart"),     params = list(xval = 3L)),
      ranger = list(id = f("ranger"),    params = list(num.trees = 50))
    )

    imap(learners, function(learner, name) {
      insert_named(learner, list(name = name))
    })
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
  }
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
    new$learner_name = map_chr(dt$learner, function(x) x$name)
    new$task_type = task_type
    new$task_name = data_names[as.character(new$data_id)]

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
