library(mlr3)
library(mlr3misc)
library(mlr3learners)
library(data.table)
library(inferGE)
library(here)
library(batchtools)

future::plan("multicore")

lgr::get_logger("mlr3")$set_threshold("warn")


f = function(i) {
  SIZES = c(500L, 1000L, 5000L, 10000L)
  REPS_AZ = 10L
  REPS_TRUTH = 400L
  
  simulate = function(n) {
    x = rnorm(n)
    y = x + rnorm(n)
    dat = data.table(x = x, y = y)
    as_task_regr(dat, target = "y")
  }
  
  var_ests = map(SIZES, function(n) {
    var_ests = map_dbl(1:REPS_AZ, function(rep) {
      task = simulate(n)
      res_az = rsmp("austern_zhou", folds = 5L)
      res_cv = rsmp("cv", folds = 5L)
      rr_az_lm = resample(task, lrn("regr.lm"), res_az)
      rr_cv_lm = resample(task, lrn("regr.lm"), res_cv)
      ci_az_lm = infer_austern_zhou(rr_az_lm, rr_cv_lm)
      sqrt(ci_az_lm$info[[1L]]$s2_cv / n)
    })
  
    data.table(n = n, est = mean(var_ests), se_est = sd(var_ests) / sqrt(REPS_AZ))
  }) |> rbindlist()
  
  
  truths = map(SIZES, function(n) {
    truth_ests = map_dbl(1:REPS_TRUTH, function(i) {
      task = simulate(n)
      resampling = rsmp("cv", folds = 5)$instantiate(task)
      unname(resample(task, lrn("regr.lm"), resampling)$aggregate())
    })
  
    data.table(n = n, truth = sd(truth_ests))
  }) |> rbindlist()
  
  merge(var_ests, truths, by = "n")
}

reg = makeRegistry(Sys.getenv("VAR_PATH_AZ"),
  packages = c("mlr3", "mlr3misc", "data.table", "future", "inferGE", "mlr3learners")
)

batchMap(i = 1, f)

submitJobs(1, resources = list(ncpus = 8L, memory = 512 * 64))
