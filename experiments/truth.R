library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/run5"
EXPERIMENT_REG = loadRegistry(EXPERIMENT_PATH, make.default = FALSE)
EXPERIMENT_TBL = unwrap(getJobTable(reg = EXPERIMENT_REG))

TRUTH_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/truth"
TRUTH_REG = makeRegistry(TRUTH_PATH,
  packages = "data.table"
)

tbl = getJobTable(reg = EXPERIMENT_REG) |>
  unwrap()

jt = jt[list("insample"), on = "resampling_name"]


batchExport(list(jt = jt, EXPERIMENT_REG = EXPERIMENT_REG))

batchMap(seq_len(nrow(jt)), function(i) {
  job_id = jt[i, "job.id"][[1L]]
  reg = loadResult(job_id, reg = EXPERIMENT_REG)
  task_name = jt[i, "task_name"][[1L]]
  size = jt[i, "size"][[1L]]
  repl = jt[i, "repl"][[1L]]

  truth = reg$holdout_scores

  cbind(truth, data.table(
    task = task_name,
    size = jt[i, "size"][[1L]],
    repl = jt[i, "repl"][[1L]]
  ))
})
