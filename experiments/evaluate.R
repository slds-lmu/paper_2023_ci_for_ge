library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/run1"
EVAL_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/eval"

EVAL_REG = makeRegistry(EVAL_PATH,
  packages = c("inferGE", "mlr3misc", "mlr3", "digest", "withr", "uuid"),
  seed = 1L,
)

EVAL_CONFIG = list(
  oob_50           = list(infer = infer_oob,            args = list(x = "bootstrap_50")),
  oob_100          = list(infer = infer_oob,            args = list(x = "bootstrap_100")),
  diettrich        = list(infer = infer_52cv,           args = list(x = "diettrich")),
  bayle_10         = list(infer = infer_bayle,          args = list(x = "cv_10")),
  bates            = list(infer = infer_bates           args = list(x = "nested_cv")),
  ls_bootstrap_50  = list(infer = infer_ls_boot,        args = list(x = "bootstrap_50", y = "insample"))
  ls_bootstrap_100 = list(infer = infer_ls_boot,        args = list(x = "bootstrap_100", y = "insample")),
  "632plus_50"     = list(infer = infer_632plus,        args = list(x = "bootstrap_50", y = "insample")),
  "632plus_100"    = list(infer = infer_632plus,        args = list(x = "bootstrap_100", y = "insample")),
  standard_holdout = list(infer = infer_holdout,        args = list(x = "holdout")),
  corrected_t      = list(infer = infer_corrected_t,    args = list(x = "subsampling_10")),
  austern_zhou     = list(infer = infer_austern_zhou,   args = list(x = "austern_zhou")),
  ts_bootstrap     = list(infer = infer_ts_boot,        args = list(x = "two_stage")),
  bccv             = list(infer = infer_bootstrap_ccv,  args = list(x = "bootstrap_ccv")),
  bccv_bias        = list(infer = infer_bootstrap_ccv,  args = list(x = "bootstrap_ccv", y = "loo")),
  conservative_z   = list(infer = infer_conservative_z, args = list(x = "conservative_z"))
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

submitJobs(1)

