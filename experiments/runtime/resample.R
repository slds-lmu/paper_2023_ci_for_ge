library(batchtools)
library(mlr3misc)
library(mlr3oml)
library(duckdb)
library(inferGE)
library(data.table)

source(here::here("experiments", "helper.R"))

if (is.null(getOption("mlr3oml.cache")) || isFALSE(getOption("mlr3oml.cache"))) {
  stop("Please configure the option mlr3oml.cache to TRUE or a specific path.")
}

TASKS <- list(
  classif = c(45570, 45689, 45704, 45654, 45665, 45668, 45669, 45672, 45693),
  regr = c(45692, 45694, 45655, 45666, 45667, 45670, 45671, 45695, 45696)
)

data_names <- list(
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
  "45694" = "physiochemical_protein",
  "45655" = "bates_regr_20",
  "45666" = "friedman1",
  "45667" = "bates_regr_100",
  "45670" = "chen_10_null",
  "45671" = "chen_10",
  "45695" = "sgemm_gpu",
  "45696" = "video_transcoding"
)

SEED <- 42
N_REP <- 1L

REGISTRY_PATH <- Sys.getenv("RESAMPLE_PATH_RUNTIME")

reg <- makeExperimentRegistry(
  file.dir = REGISTRY_PATH,
  seed = SEED,
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr"),
  work.dir = here::here()
)

RESAMPLINGS <- list(other = list(
  # holdout
  #holdout_50         = list(id = "holdout", params = list(ratio = 0.5)),
  #holdout_60         = list(id = "holdout", params = list(ratio = 0.6)),
  holdout_66         = list(id = "holdout", params = list(ratio = 2 / 3)),
  #holdout_70         = list(id = "holdout", params = list(ratio = 0.7)),
  #holdout_80         = list(id = "holdout", params = list(ratio = 0.8)),
  holdout_90         = list(id = "holdout", params = list(ratio = 0.9)),
  # cross validation
  cv_5               = list(id = "cv", params = list(folds = 5)),
  cv_10              = list(id = "cv", params = list(folds = 10)),
  #cv_25              = list(id = "cv", params = list(folds = 25)),
  #cv_50              = list(id = "cv", params = list(folds = 50)),
  #cv_75              = list(id = "cv", params = list(folds = 75)),
  #cv_100             = list(id = "cv", params = list(folds = 100)),
  subsampling_10     = list(id = "subsampling", params = list(repeats = 10, ratio = 0.9)),
  subsampling_25     = list(id = "subsampling", params = list(repeats = 25, ratio = 0.9)),
  subsampling_50     = list(id = "subsampling", params = list(repeats = 50, ratio = 0.9)),
  subsampling_100    = list(id = "subsampling", params = list(repeats = 100, ratio = 0.9)),
  #subsampling_100_80 = list(id = "subsampling", params = list(repeats = 100, ratio = 0.8)),
  #subsampling_100_70 = list(id = "subsampling", params = list(repeats = 100, ratio = 0.7)),
  # subsampling
  rep_cv_5_5         = list(id = "repeated_cv", params = list(folds = 5, repeats = 5)),
  diettrich          = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
  bootstrap_10       = list(id = "bootstrap", params = list(ratio = 1, repeats = 10)),
  bootstrap_50       = list(id = "bootstrap", params = list(ratio = 1, repeats = 50)),
  bootstrap_100      = list(id = "bootstrap", params = list(ratio = 1, repeats = 100)),
  insample           = list(id = "insample", params = list()),
  nested_cv_250      = list(id = "nested_cv", params = list(folds = 5, repeats = 10)),
  conservative_z_250 = list(id = "conservative_z", params = list(J = 10, M = 12, ratio = 0.9)),
  nested_cv_75       = list(id = "nested_cv", params = list(folds = 5, repeats = 3)),
  conservative_z_105 = list(id = "conservative_z", params = list(J = 5, M = 10, ratio = 0.9))
), small = list(
  nested_cv_5000 = list(id = "nested_cv", params = list(folds = 5, repeats = 200)),
  conservative_z_315 = list(id = "conservative_z", params = list(J = 15, M = 10, ratio = 0.9)),
  two_stage = list(id = "nested_bootstrap", params = list(reps_outer = 200, reps_inner = 10)),
  loo = list(id = "loo", params = list()),
  austern_zhou = list(id = "austern_zhou", params = list(folds = 5, repeats = 1)),
  austern_zhou_rep = list(id = "austern_zhou", params = list(folds = 5, repeats = 5)),
  bootstrap_500 = list(id = "bootstrap", params = list(ratio = 1, repeats = 500)),
  bootstrap_1000 = list(id = "bootstrap", params = list(ratio = 1, repeats = 1000)),
  conservative_z_5050 = list(id = "conservative_z", params = list(J = 50, M = 50, ratio = 0.9))
), tiny = list(
  bootstrap_ccv = list(id = "bootstrap_ccv", params = list(ratio = 1, repeats = 100))
))


SIZES <- list(
  tiny = 100L,
  small = 500L,
  other = c(1000L, 5000L, 10000L)
)

LEARNERS <- list(
  regr = list(
    list(name = "ridge", id = "regr.glmnet", params = list(alpha = 0)),
    list(name = "rpart", id = "regr.rpart", params = list()),
    list(name = "ranger", id = "regr.ranger", params = list(num.trees = 50)),
    list(name = "linear", id = "regr.lm", params = list())
  ),
  classif = list(
    list(name = "ridge", id = "classif.glmnet", params = list(alpha = 0)),
    list(name = "rpart", id = "classif.rpart", params = list()),
    list(name = "ranger", id = "classif.ranger", params = list(num.trees = 50)),
    list(name = "linear", id = "classif.log_reg", params = list())
  )
)

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


make_prob_designs <- function(type) {
  prob_designs_other <- map(c("regr", "classif"), function(task_type) {
    dt <- CJ(
      data_id = TASKS[[task_type]],
      learner = LEARNERS[[task_type]],
      size = SIZES[[type]],
      sorted = FALSE
    )

    new <- data.table()

    new$data_id <- dt$data_id
    new$size <- dt$size
    new$learner_id <- map_chr(dt$learner, "id")
    new$learner_params <- map(dt$learner, function(x) list(x$params))
    new$learner_name <- map_chr(dt$learner, function(x) x$name)
    new$task_type <- task_type
    new$task_name <- data_names[as.character(new$data_id)]

    new
  }) |> rbindlist()
}

prob_design_tiny <- make_prob_designs("tiny")
prob_design_small <- make_prob_designs("small")
prob_design_other <- make_prob_designs("other")

# Add experiments for "other" resamplings

# type is either "other" or "small"
make_algo_design <- function(type) {
  algo_designs_other <- data.table(
    resampling_id = map_chr(RESAMPLINGS[[type]], "id"),
    resampling_params = map(RESAMPLINGS[[type]], function(x) list(x$params)),
    # The resampling name is just so we can easier identify the resamplings later
    resampling_name = names(RESAMPLINGS[[type]])
  )
}

algo_design_tiny <- make_algo_design("tiny")
algo_design_small <- make_algo_design("small")
algo_design_other <- make_algo_design("other")


# Applying all tiny resampling methods to tiny problems
addExperiments(
  algo.designs = list(run_resampling = algo_design_tiny),
  prob.designs = list(ci_estimation = prob_design_tiny),
  repls = N_REP
)

# Applying all small algos to tiny and small problems
addExperiments(
  algo.designs = list(run_resampling = algo_design_small),
  prob.designs = list(ci_estimation = prob_design_tiny),
  repls = N_REP
)
addExperiments(
  algo.designs = list(run_resampling = algo_design_small),
  prob.designs = list(ci_estimation = prob_design_small),
  repls = N_REP
)

# Applying all other resampling methods to tiny, small and other problems
addExperiments(
  algo.designs = list(run_resampling = algo_design_other),
  prob.designs = list(ci_estimation = prob_design_tiny),
  repls = N_REP
)
addExperiments(
  algo.designs = list(run_resampling = algo_design_other),
  prob.designs = list(ci_estimation = prob_design_small),
  repls = N_REP
)
addExperiments(
  algo.designs = list(run_resampling = algo_design_other),
  prob.designs = list(ci_estimation = prob_design_other),
  repls = N_REP
)
