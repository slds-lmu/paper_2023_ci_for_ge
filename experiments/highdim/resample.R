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

TASKS = paste0("highdim_", 100 * 2^c(0, 1, 2, 3, 4, 5, 6))

SEED <- 42
N_REP <- 500L

REGISTRY_PATH <- Sys.getenv("RESAMPLE_PATH_HIGHDIM")

reg <- makeExperimentRegistry(
  file.dir = REGISTRY_PATH,
  seed = SEED,
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr", "mlr3mbo", "mlr3tuning", "mlr3torch"),
  work.dir = here::here()
)

RESAMPLINGS <- list(
  subsampling_25_90  = list(id = "subsampling", params = list(repeats = 25, ratio = 0.9)),
  nested_cv_75       = list(id = "nested_cv", params = list(folds = 5, repeats = 3)),
  # J is inner reps, M is outer reps
  conservative_z_105 = list(id = "conservative_z", params = list(J = 5, M = 10, ratio = 0.9)),
  insample           = list(id = "insample", params = list())
)

LEARNERS <- list(
  list(name = "lasso", id = "classif.cv_glmnet", params = list(alpha = 1)),
  list(name = "random_forest", id = "classif.ranger", params = list(num.trees = 50))
)

batchExport(list(
  make_task = make_task,
  make_learner = make_learner,
  make_resampling = make_resampling,
  make_task_highdim = make_task_highdim
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


make_prob_designs <- function() {

  dt <- CJ(
    data_id = TASKS,
    learner = LEARNERS,
    size = 1000L,
    sorted = FALSE
  )

  new <- data.table()

  new$data_id <- dt$data_id
  new$size <- dt$size
  new$learner_id <- map_chr(dt$learner, "id")
  new$learner_params <- map(dt$learner, function(x) list(x$params))
  new$learner_name <- map_chr(dt$learner, function(x) x$name)
  new$task_type <- "classif"
  new$task_name <- dt$data_id

  new
}

prob_design <- make_prob_designs()

# Add experiments for "other" resamplings

# type is either "other" or "small"
make_algo_design <- function() {
  algo_designs_other <- data.table(
    resampling_id = map_chr(RESAMPLINGS, "id"),
    resampling_params = map(RESAMPLINGS, function(x) list(x$params)),
    # The resampling name is just so we can easier identify the resamplings later
    resampling_name = names(RESAMPLINGS)
  )
}

algo_design <- make_algo_design()


# Applying all tiny resampling methods to tiny problems
addExperiments(
  algo.designs = list(run_resampling = algo_design),
  prob.designs = list(ci_estimation = prob_design),
  repls = N_REP
)
