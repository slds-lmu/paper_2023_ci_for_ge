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

TASKS = list(
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
  "45694" = "physiochemical_protein",
  "45655" = "bates_regr_20",
  "45666" = "friedman1",
  "45667" = "bates_regr_100",
  "45670" = "chen_10_null",
  "45671" = "chen_10",
  "45695" = "sgemm_gpu",
  "45696" = "video_transcoding"
)

SEED = 42
N_REP = 500L

REGISTRY_PATH = Sys.getenv("RESAMPLE_PATH_AUSTERN")

reg = loadRegistry(REGISTRY_PATH, writeable = TRUE)
#reg = makeExperimentRegistry(
#  file.dir = REGISTRY_PATH,
#  seed = SEED,
#  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr"),
#  work.dir = here::here()
#)

RESAMPLINGS = list(
  insample = list(id = "insample",         params = list()),
  cv_5 =     list(id = "cv",               params = list(folds = 5L))
)

SIZES = list(250)

LEARNERS = list(
  regr = list(
    list(name = "ridge",  id = "regr.cv_glmnet", params = list(alpha = 0, nfolds = 3L)),
    list(name = "rpart",  id = "regr.rpart",     params = list()),
    list(name = "ranger", id = "regr.ranger",    params = list(num.trees = 50))
  ),
  classif = list(
    list(name = "ridge",  id = "classif.cv_glmnet", params = list(alpha = 0, nfolds = 3L)),
    list(name = "rpart",  id = "classif.rpart",     params = list()),
    list(name = "ranger", id = "classif.ranger",    params = list(num.trees = 50))
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


prob_design = map(c("regr", "classif"), function(task_type) {
  dt = CJ(
    data_id = TASKS[[task_type]],
    learner = LEARNERS[[task_type]],
    size = SIZES,
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

algo_design = data.table(
  resampling_id = map_chr(RESAMPLINGS, "id"),
  resampling_params = map(RESAMPLINGS, function(x) list(x$params)),
  # The resampling name is just so we can easier identify the resamplings later
  resampling_name = names(RESAMPLINGS)
)

addExperiments(
  algo.designs = list(run_resampling = algo_design),
  prob.designs = list(ci_estimation = prob_design),
  repls = N_REP
)
