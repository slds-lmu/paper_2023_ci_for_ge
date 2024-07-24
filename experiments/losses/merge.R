library(batchtools)
library(data.table)
library(mlr3misc)


reg = makeRegistry("/gscratch/sfische6/benchmarks/ci_for_ge/final_ci_losses_merged", packages = c("batchtools", "mlr3misc", "data.table"))

paths = c("ORIGINAL", "LM", "RIDGE")

batchMap(path = paths, function(path) {
  reg = loadRegistry(Sys.getenv(paste0("CI_PATH_LOSSES_", path)))
  rbindlist(map(findDone()[[1]], loadResult), fill = TRUE)
})

submitJobs()
waitForJobs()

tbl = rbindlist(map(findDone()[[1]], loadResult), fill = TRUE)


