library(batchtools)
library(mlr3misc)

reg_truth = loadRegistry(Sys.getenv("TRUTH_PATH_MLP"), make.default = FALSE, conf.file = here::here("experiments", "mlp", "batchtools.conf.R"))
getStatus(reg = reg_truth)
ids_truth = findDone(reg = reg_truth)[[1]]
reg_ci    = loadRegistry(Sys.getenv("CI_PATH_MLP"), make.default = FALSE, conf.file = here::here("experiments", "mlp", "batchtools.conf.R"))
getStatus(reg = reg_ci)
ids_ci    = findDone(reg = reg_ci)[[1]]

truth = rbindlist(lapply(ids_truth, function(id) {
  loadResult(id, reg = reg_truth)
}))

ci = rbindlist(lapply(ids_ci, function(id) {
  loadResult(id, reg = reg_ci)
}))

truth = truth[size == 5000, ]
truth$size = NULL
truth$learner = NULL
truth[, ER := mean(R), by = .(task, measure)]

ci = ci[size == 5000, ]

tbl = merge(truth, ci, by = c("task", "repl", "measure"))
tbl$method = mlr3misc::map_chr(tbl$method, function(x) {
  if (x == "conservative_z_105") {
    "conz_10_5"
  } else if (x == "nested_cv_75") {
    "ncv_3_5"
  } else if (x == "corrected_t_25") {
    "cort_25"
  }
})
tbl$width = tbl$upper - tbl$lower
setnames(tbl, "measure", "loss")

tbl_aggr = tbl[, list(
  ER = ER[[1L]],
  cov_R = mean(lower <= R & upper >= R),
  cov_ER = mean(lower <= ER & upper >= ER),
  iters = iters[[1]],
  width_median = median(width),
  width_mean = mean(width)
), by = .(task, loss, method, size)]

tbl$resampling = NULL

saveRDS(tbl_aggr, "~/mlp_aggr.rds")
saveRDS(tbl, "~/mlp.rds")

