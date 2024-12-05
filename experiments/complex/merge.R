library(batchtools)
library(mlr3misc)

reg_truth = loadRegistry(Sys.getenv("TRUTH_PATH_HIGHDIM"), make.default = FALSE)
getStatus(reg = reg_truth)
ids_truth = findDone(reg = reg_truth)[[1]]
reg_ci    = loadRegistry(Sys.getenv("CI_PATH_HIGHDIM"), make.default = FALSE)
getStatus(reg = reg_ci)
ids_ci    = findDone(reg = reg_ci)[[1]]

truth = rbindlist(lapply(ids_truth, function(id) {
  loadResult(id, reg = reg_truth)
}))

ci = rbindlist(lapply(ids_ci, function(id) {
  loadResult(id, reg = reg_ci)
}))
