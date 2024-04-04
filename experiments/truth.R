library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = Sys.getenv("RESAMPLE_PATH")
EXPERIMENT_REG = loadRegistry(EXPERIMENT_PATH, make.default = FALSE)
EXPERIMENT_TBL = unwrap(getJobTable(reg = EXPERIMENT_REG))

TRUTH_PATH = Sys.getenv("TRUTH_PATH")
TRUTH_REG = makeRegistry(TRUTH_PATH,
  packages = c("data.table", "batchtools")
)

jt = getJobTable(reg = EXPERIMENT_REG) |>
  unwrap()

jt = jt[list("insample"), on = "resampling_name"]

batchExport(list(jt = jt, EXPERIMENT_REG = EXPERIMENT_REG))

batchMap(i = seq_len(nrow(jt)), fun = function(i) {
  job_id = jt[i, "job.id"][[1L]]
  learner_id = jt[i, "learner_id"][[1L]]
  reg = loadResult(job_id, reg = EXPERIMENT_REG)

  truth = reg$holdout_scores


  task_type = if (startsWith(learner_id, "classif")) "classif" else "regr"
  tbl = cbind(truth, data.table(
    task = jt[i, "task_name"][[1L]],
    size = jt[i, "size"][[1L]],
    repl = jt[i, "repl"][[1L]],
    task_type = task_type,
    learner = jt[i, "learner_name"][[1L]]
  ))

  measure_vars = if (task_type == "classif") {
    c("zero_one", "logloss", "bbrier")
  } else {
	  # FIXME rename to percentual_mse after next run
    c("se", "ae", "standardized_se", "percentual_mse")
  }


  browser()
  melt(tbl, id.vars = c("task", "size", "repl", "learner"), measure.vars = measure_vars,
    variable.name = "measure", value.name = "R")
}, reg = TRUTH_REG)

jt_truth = getJobTable(reg = TRUTH_REG)
chunks = data.table(
  job.id = jt_truth$job.id,
  chunk = batchtools::chunk(x = jt_truth$job.id, chunk.size = 100)
)
