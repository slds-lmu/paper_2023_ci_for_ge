library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/run_big4"
EXPERIMENT_REG = loadRegistry(EXPERIMENT_PATH, make.default = FALSE)
EXPERIMENT_TBL = unwrap(getJobTable(reg = EXPERIMENT_REG))

TRUTH_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/truth"
TRUTH_REG = makeRegistry(TRUTH_PATH,
  packages = c("data.table", "batchtools")
)

jt = getJobTable(reg = EXPERIMENT_REG) |>
  unwrap()

jt = jt[list("insample"), on = "resampling_name"]

batchExport(list(jt = jt, EXPERIMENT_REG = EXPERIMENT_REG))

batchMap(i = seq_len(nrow(jt)), fun = function(i) {
  job_id = jt[i, "job.id"][[1L]]
  reg = loadResult(job_id, reg = EXPERIMENT_REG)

  truth = reg$holdout_scores

  cbind(truth, data.table(
    task = jt[i, "task_name"][[1L]],
    size = jt[i, "size"][[1L]],
    repl = jt[i, "repl"][[1L]],
    learner = jt[i, "learner_name"][[1L]]
  ))
})

jt_truth = getJobTable(reg = TRUTH_REG)
chunks = data.table(
  job.id = jt_truth$job.id,
  chunk = batchtools::chunk(x = jt_truth$job.id, chunk.size = 100)
)
