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

# only run the network on non-linear DPGs
TASKS$regr = TASKS$regr[TASKS$regr %nin% c(45667, 45655, 45670)]
TASKS$classif = TASKS$classif[TASKS$classif %nin% c(45654, 45665, 45668, 45669, 45672)]

data_names <- list(
  "45570" = "higgs",
  "45689" = "adult",
  "45704" = "covertype",
  #"45654" = "bates_classif_20",
  #"45665" = "colon",
  #"45668" = "bates_classif_100",
  #"45669" = "breast",
  #"45672" = "prostate",
  "45693" = "electricity",
  "45692" = "diamonds",
  "45694" = "physiochemical_protein",
  #"45655" = "bates_regr_20",
  "45666" = "friedman1",
  #"45667" = "bates_regr_100",
  # No method performs well here
  #"45670" = "chen_10_null",
  "45671" = "chen_10",
  "45695" = "sgemm_gpu",
  "45696" = "video_transcoding"
)

SEED <- 42
# Leave it at 500 for now but we will probably not run all repetitions as it is so expensive
N_REP <- 500L

REGISTRY_PATH <- Sys.getenv("RESAMPLE_PATH_MLP")

reg <- makeExperimentRegistry(
  file.dir = REGISTRY_PATH,
  seed = SEED,
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr", "mlr3mbo", "mlr3tuning", "mlr3torch"),
  conf.file = here::here("experiments", "mlp", "batchtools.conf.R"),
  work.dir = here::here(),

)

RESAMPLINGS <- list(other = list(
  subsampling_25_90  = list(id = "subsampling", params = list(repeats = 25, ratio = 0.9)),
  nested_cv_75       = list(id = "nested_cv", params = list(folds = 5, repeats = 3)),
  # J is inner reps, M is outer reps
  conservative_z_105 = list(id = "conservative_z", params = list(J = 5, M = 10, ratio = 0.9)),
  insample           = list(id = "insample", params = list())
))

# Run the network only on 'relatively' large problems
SIZES <- list(
  other = c(1000L, 5000L, 10000L)
)

LEARNERS <- list(
  regr = list(
    list(name = "mlp", id = "regr.mlp", params = list(patience = 20L, batch_size = 512, drop_last = FALSE))
  ),
  classif = list(
    list(name = "mlp", id = "classif.mlp", params = list(patience = 20L, batch_size = 512, drop_last = FALSE))
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

prob_design <- make_prob_designs("other")

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

algo_design <- make_algo_design("other")


# Applying all tiny resampling methods to tiny problems
addExperiments(
  algo.designs = list(run_resampling = algo_design),
  prob.designs = list(ci_estimation = prob_design),
  repls = N_REP
)
