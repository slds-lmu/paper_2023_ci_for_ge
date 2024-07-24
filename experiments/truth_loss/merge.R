library(batchtools)
library(mlr3misc)
library(data.table)

reg = loadRegistry(Sys.getenv("TRUTH_PATH_LOSSES_RESAMPLE"))

jt = unwrap(getJobTable())
jt = jt[, c("size", "repl", "learner_name", "task_name")]

dt = map(findDone()[[1]], function(i) {
  loadResult(i)$holdout_scores
})


dt = cbind(dt, jt)
saveRDS(dt, "~/truth_losses.rds")

