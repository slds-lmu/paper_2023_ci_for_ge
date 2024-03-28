library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/run_big4"
EVAL_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/eval_big4"

EVAL_REG = if (file.exists(EVAL_PATH)) {
  loadRegistry(EVAL_PATH, writeable = TRUE)
} else {
  makeRegistry(EVAL_PATH,
    packages = c("inferGE", "mlr3misc", "mlr3", "digest", "withr", "uuid", "batchtools", "tictoc", "duckdb", "mlr3pipelines", "mlr3learners", "ranger", "mlr3oml"),
    seed = 1L,
  )
}
EXPERIMENT_REG = loadRegistry(EXPERIMENT_PATH, make.default = FALSE)
EXPERIMENT_TBL = unwrap(getJobTable(reg = EXPERIMENT_REG))

# order is as in experiments/design.R
# we use the default alpha = 0.05 everywhere
# 1: name (unique)
# 2: inference method
# 3: named identifiers for the resampling_named from the experiment's job table
# 4: additional parameters passed to the resampling method
EVAL_CONFIG = list(
  ## other

  # holdout
  list("standard_holdout",   "infer_holdout",        list(x = "holdout"),                       list()),

  # subsampling_10
  list("corrected_t_10",     "infer_corrected_t",    list(x = "subsampling_10"),                list()),

  # subsampling_50
  list("corrected_t_50",     "infer_corrected_t",    list(x = "subsampling_50"),                list()),

  # cv_10
  list("bayle_10_within",    "infer_bayle",          list(x = "cv_10"),                         list(variance = "within-fold")),
  list("bayle_10_all_pairs", "infer_bayle",          list(x = "cv_10"),                         list(variance = "all-pairs")),

  # diettrich
  list("diettrich",          "infer_52cv",           list(x = "diettrich"),                     list()),

  # bootstrap_50
  list("oob_50",             "infer_oob",            list(x = "bootstrap_50"),                  list()),
  list("ls_bootstrap_50",    "infer_ls_boot",        list(x = "bootstrap_50", y = "insample"),  list()),
  list("632plus_50",         "infer_632plus",        list(x = "bootstrap_50", y = "insample"),  list()),

  # bootstrap_100
  list("oob_100",            "infer_oob",            list(x = "bootstrap_100"),                 list()),
  list("ls_bootstrap_100",   "infer_ls_boot",        list(x = "bootstrap_100", y = "insample"), list()),
  list("632plus_100",        "infer_632plus",        list(x = "bootstrap_100", y = "insample"), list()),

  # insample
  # also used for other resam"pling methods

  ## small

  # nested cv
  list("nested_cv",          "infer_bates",          list(x = "nested_cv"),                     list()),

  # conservative_z
  list("conservative_z",     "infer_conservative_z", list(x = "conservative_z"),                list()),

  # nested_bootstrap
  list("ts_bootstrap",       "infer_ts_boot",        list(x = "two_stage"),                     list()),

  # loo
  list("bayle_loo",          "infer_bayle",          list(x = "loo"),                           list(variance = "all-pairs")),

  # austern_zhou
  list("austern_zhou",       "infer_austern_zhou",   list(x = "austern_zhou"),                  list()),

  list("austern_zhou_rep",   "infer_austern_zhou",   list(x = "austern_zhou_rep"),              list()),

  # bccv
  list("bccv",               "infer_bootstrap_ccv",  list(x = "bootstrap_ccv"),                 list()),
  list("bccv_bias",          "infer_bootstrap_ccv",  list(x = "bootstrap_ccv", y = "loo"),      list())
)

tbl1 = rbindlist(map(EVAL_CONFIG, function(cfg) {
  cfg[[3]] = list(cfg[[3]])
  cfg[[4]] = list(cfg[[4]])
  as.data.table(cfg)
}))
names(tbl1) = c("name", "inference_method", "rrs", "args")

# now we create the table that contains the job ids
tbl2 = map_dtr(EVAL_CONFIG, function(cfg) {
  resampling_names = cfg[[3]]
  name = cfg[[1]]
  inference = cfg[[1]]
  # primary resampling name
  rn1 = resampling_names[[1]]
  keep_cols = c("data_id", "size", "repl", "learner_id", "resampling_name", "job.id", "task_name")
  tbl = EXPERIMENT_TBL[list(rn1), ..keep_cols,  on = "resampling_name"]

  if (length(resampling_names) == 2) {
    rn2 = resampling_names[[2]]
    tbl = merge(tbl, EXPERIMENT_TBL[list(rn2), ..keep_cols, on = "resampling_name"], by = c("data_id", "size", "repl", "learner_id", "task_name"))
    setnames(tbl, c("job.id.x", "job.id.y", "resampling_name.x", "resampling_name.y"), c("x", "y", "resampling_name_x", "resampling_name_y"))
  } else {
    setnames(tbl, c("job.id", "resampling_name"), c("x", "resampling_name_x"))
    tbl$y = NA
    tbl$resampling_name_y = NA
  }
  tbl$name = cfg[[1]]
  tbl$inference = cfg[[2]]
  tbl$args = rep(list(cfg[[4]]), times = nrow(tbl))

  tbl
})

batchExport(list(
  EXPERIMENT_PATH = EXPERIMENT_PATH,
  EXPERIMENT_REG = EXPERIMENT_REG,
  EXPERIMENT_TBL = EXPERIMENT_TBL,
  make_resample_result = make_resample_result,
  make_task = make_task,
  make_learner = make_learner,
  calculate_ci = calculate_ci,
  make_resampling = make_resampling,
  tbl2 = tbl2
  ),
  reg = EVAL_REG)

batchMap(i = seq_len(nrow(tbl2)), fun =  function(i) {
  name = tbl2[i, "name"][[1]]
  inference = getFromNamespace(tbl2[i, "inference"][[1]], ns = "inferGE")
  x = tbl2[i, "x"][[1]]
  y = tbl2[i, "y"][[1]]
  args = tbl2[i, "args"][[1]][[1]]
  learner_id = tbl2[i, "learner_id"][[1]]
  task_name = tbl2[i, "task_name"][[1]]
  size = tbl2[i, "size"][[1]]
  repl = tbl2[i, "repl"][[1]]

  calculate_ci(
    name = name,
    inference = inference,
    x = x,
    y = y,
    args = args,
    learner_id = learner_id,
    task_name = task_name,
    size = size,
    repl = repl
  )
})

