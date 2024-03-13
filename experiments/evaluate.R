library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/run5"
EVAL_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/eval18"

EVAL_REG = if (file.exists(EVAL_PATH)) {
  loadRegistry(EVAL_PATH, writeable = TRUE)
} else {
  makeRegistry(EVAL_PATH,
    packages = c("inferGE", "mlr3misc", "mlr3", "digest", "withr", "uuid", "mlr3measures"),
    seed = 1L,
  )
}


EVAL_CONFIG = list(
  list("oob_50",           infer_oob,            list(x = "bootstrap_50")),
  list("oob_100",          infer_oob,            list(x = "bootstrap_100")),
  list("diettrich",        infer_52cv,           list(x = "diettrich")),
  list("bayle_10",         infer_bayle,          list(x = "cv_10")),
  list("bates",            infer_bates,          list(x = "nested_cv")),
  list("ls_bootstrap_50",  infer_ls_boot,        list(x = "bootstrap_50", y = "insample")),
  list("ls_bootstrap_100", infer_ls_boot,        list(x = "bootstrap_100", y = "insample")),
  list("632plus_50",       infer_632plus,        list(x = "bootstrap_50", y = "insample")),
  list("632plus_100",      infer_632plus,        list(x = "bootstrap_100", y = "insample")),
  list("standard_holdout", infer_holdout,        list(x = "holdout")),
  list("corrected_t",      infer_corrected_t,    list(x = "subsampling_10")),
  list("austern_zhou",     infer_austern_zhou,   list(x = "austern_zhou")),
  list("ts_bootstrap",     infer_ts_boot,        list(x = "two_stage")),
  list("bccv",             infer_bootstrap_ccv,  list(x = "bootstrap_ccv")),
  list("bccv_bias",        infer_bootstrap_ccv,  list(x = "bootstrap_ccv", y = "loo")),
  list("conservative_z",   infer_conservative_z, list(x = "conservative_z"))
)

batchExport(list(
  EXPERIMENT_PATH = EXPERIMENT_PATH,
  EVAL_CONFIG = EVAL_CONFIG,
  make_resample_result = make_resample_result,
  make_task = make_task,
  make_learner = make_learner,
  make_resampling = make_resampling
  ),
  reg = EVAL_REG)

batchMap(calculate_ci, config = EVAL_CONFIG)

