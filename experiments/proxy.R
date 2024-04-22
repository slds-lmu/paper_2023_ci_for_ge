library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = Sys.getenv("RESAMPLE_PATH")
EXPERIMENT_REG = loadRegistry(EXPERIMENT_PATH, make.default = FALSE)
EXPERIMENT_TBL = unwrap(getJobTable(reg = EXPERIMENT_REG))

PROXY_PATH = Sys.getenv("PROXY_PATH")
PROXY_REG = makeRegistry(PROXY_PATH,
  packages = c("data.table", "batchtools")
)

jt = getJobTable(reg = EXPERIMENT_REG) |>
  unwrap()

jt = jt[map_lgl(jt$resampling_name, function(n) n %in% c("holdout_66", "holdout_90", "cv_5", "cv_10", "rep_cv_5_5", "loo")), ]

batchExport(list(jt = jt, EXPERIMENT_REG = EXPERIMENT_REG))

batchMap(i = seq_len(nrow(jt)), fun = function(i) {
  job_id = jt[i, "job.id"][[1L]]
  learner_id = jt[i, "learner_id"][[1L]]
  reg = loadResult(job_id, reg = EXPERIMENT_REG)

  proxy = reg$holdout_scores[, lapply(.SD, mean)]

  task_type = if (startsWith(learner_id, "classif")) "classif" else "regr"
  tbl = cbind(proxy, data.table(
    task = jt[i, "task_name"][[1L]],
    size = jt[i, "size"][[1L]],
    repl = jt[i, "repl"][[1L]],
    resampling = jt[i, "resampling_name"][[1L]],
    task_type = task_type,
    learner = jt[i, "learner_name"][[1L]]
  ))

  measure_vars = if (task_type == "classif") {
    c("zero_one", "logloss", "bbrier")
  } else {
	  # FIXME rename to percentual_mse after next run
    c("se", "ae", "standardized_se", "percentual_se")
  }

  melt(tbl, id.vars = c("task", "size", "repl", "learner", "resampling"), measure.vars = measure_vars,
    variable.name = "measure", value.name = "PQ")
}, reg = PROXY_REG)

jt_proxy = getJobTable(reg = PROXY_REG)
chunks = data.table(
  job.id = jt_proxy$job.id,
  chunk = batchtools::chunk(x = jt_proxy$job.id, chunk.size = 100)
)
