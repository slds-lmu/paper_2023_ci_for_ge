library(batchtools)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = Sys.getenv("RESAMPLE_PATH_LM")
EVAL_PATH = Sys.getenv("CI_PATH_LM")

EVAL_REG = makeRegistry(EVAL_PATH,
  packages = c("inferGE", "mlr3misc", "mlr3", "digest", "withr", "uuid", "batchtools", "tictoc", "duckdb", "mlr3pipelines", "mlr3learners", "ranger", "mlr3oml", "data.table"),
  seed = 1L,
)
 
EXPERIMENT_REG = loadRegistry(EXPERIMENT_PATH, make.default = FALSE, writeable = FALSE)
EXPERIMENT_TBL = unwrap(getJobTable(reg = EXPERIMENT_REG))

# order is as in experiments/design.R
# we use the default alpha = 0.05 everywhere
# 1: name (unique)
# 2: inference method
# 3: named identifiers for the resampling_named from the experiment's job table
# 4: additional parameters passed to the resampling method
EVAL_CONFIG = list(
  ## other

  # holdout_66 and holdout_90
  list("holdout_66",   "infer_holdout",              list(x = "holdout_66"),                                    list()),
  list("holdout_90",   "infer_holdout",              list(x = "holdout_90"),                                    list()),

  # subsampling_10, 50 and 100
  list("corrected_t_10",     "infer_corrected_t",    list(x = "subsampling_10"),                                list()),
  list("corrected_t_50",     "infer_corrected_t",    list(x = "subsampling_50"),                                list()),
  list("corrected_t_100",    "infer_corrected_t",    list(x = "subsampling_100"),                               list()),

  # cv_5
  list("bayle_5_within",    "infer_bayle",           list(x = "cv_5"),                                          list(variance = "within-fold")),
  list("bayle_5_all_pairs", "infer_bayle",           list(x = "cv_5"),                                          list(variance = "all-pairs")),

  # cv_10
  list("bayle_10_within",    "infer_bayle",          list(x = "cv_10"),                                         list(variance = "within-fold")),
  list("bayle_10_all_pairs", "infer_bayle",          list(x = "cv_10"),                                         list(variance = "all-pairs")),

  # rep_cv_5_10 only used for Austern & Zhou

  # diettrich
  list("diettrich",          "infer_52cv",           list(x = "diettrich"),                                     list()),

  # bootstrap_10
  # ls-bootstrap needs more than 10 repetitions, so don't calculate it here
  list("oob_10",             "infer_oob",            list(x = "bootstrap_10"),                                  list()),
  list("632plus_10",         "infer_632plus",        list(x = "bootstrap_10", y = "insample"),                  list()),

  # bootstrap_50 and 100
  list("oob_50",             "infer_oob",            list(x = "bootstrap_50"),                                  list()),
  list("ls_bootstrap_50",    "infer_ls_boot",        list(x = "bootstrap_50", y = "insample"),                  list()),
  list("632plus_50",         "infer_632plus",        list(x = "bootstrap_50", y = "insample"),                  list()),

  list("oob_100",            "infer_oob",            list(x = "bootstrap_100"),                                 list()),
  list("ls_bootstrap_100",   "infer_ls_boot",        list(x = "bootstrap_100", y = "insample"),                 list()),
  list("632plus_100",        "infer_632plus",        list(x = "bootstrap_100", y = "insample"),                 list()),

  # insample
  # also used for other resampling methods

  ## small

  # nested cv
  list("nested_cv",          "infer_bates",          list(x = "nested_cv"),                                     list()),

  # conservative_z
  list("conservative_z",     "infer_conservative_z", list(x = "conservative_z"),                                list()),

  # nested_bootstrap
  list("ts_bootstrap",       "infer_ts_boot",        list(x = "two_stage", y = "bootstrap_10", z = "insample"), list()),

  # loo
  list("bayle_loo",          "infer_bayle",          list(x = "loo"),                                           list(variance = "all-pairs")),

  # austern_zhou
  list("austern_zhou",       "infer_austern_zhou",   list(x = "austern_zhou", y = "cv_5"),                      list()),

  list("austern_zhou_rep",   "infer_austern_zhou",   list(x = "austern_zhou_rep", y = "rep_cv_5_5"),            list()),

  # bccv
  list("bccv",               "infer_bootstrap_ccv",  list(x = "bootstrap_ccv"),                                 list()),
  list("bccv_bias",          "infer_bootstrap_ccv",  list(x = "bootstrap_ccv", y = "loo"),                      list()),

  # oob
  list("oob_500",            "infer_oob",            list(x = "bootstrap_500"),                                 list()),
  list("oob_1000",           "infer_oob",            list(x = "bootstrap_1000"),                                list())
  # 632plus
  list("632plus_500",        "infer_632plus",        list(x = "bootstrap_500", y = "insample"),                 list()),
  list("632plus_1000",       "infer_632plus",        list(x = "bootstrap_1000", y = "insample"),                list())
)

# now we create the table that contains the job ids
tbl2 = map_dtr(EVAL_CONFIG, function(cfg) {
  resampling_names = cfg[[3]]
  name = cfg[[1]]
  inference = cfg[[1]]
  # primary resampling name
  rn1 = resampling_names[[1]]
  keep_cols = c("data_id", "size", "repl", "learner_name", "resampling_name", "job.id", "task_name")
  tbl = EXPERIMENT_TBL[list(rn1), ..keep_cols,  on = "resampling_name"]

  if (length(resampling_names) == 3) {
    rn2 = resampling_names[[2]]
    rn3 = resampling_names[[3]]
    tbl = merge(tbl, EXPERIMENT_TBL[list(rn2), ..keep_cols, on = "resampling_name"], by = c("data_id", "size", "repl", "learner_name", "task_name"))
    tbl = merge(tbl, EXPERIMENT_TBL[list(rn3), ..keep_cols, on = "resampling_name"], by = c("data_id", "size", "repl", "learner_name", "task_name"))

    setnames(tbl,
      c("job.id.x", "job.id.y", "resampling_name.x", "resampling_name.y", "job.id", "resampling_name"),
      c("x", "y", "resampling_name_x", "resampling_name_y", "z", "resampling_name_z")
    )

  } else if (length(resampling_names) == 2) {
    rn2 = resampling_names[[2]]
    tbl = merge(tbl, EXPERIMENT_TBL[list(rn2), ..keep_cols, on = "resampling_name"], by = c("data_id", "size", "repl", "learner_name", "task_name"))
    setnames(tbl,
      c("job.id.x", "job.id.y", "resampling_name.x", "resampling_name.y"),
      c("x", "y", "resampling_name_x", "resampling_name_y")
    )
    tbl$z = NA
    tbl$resampling_name_z = NA
  } else {
    setnames(tbl, c("job.id", "resampling_name"), c("x", "resampling_name_x"))
    tbl$y = NA
    tbl$resampling_name_y = NA
    tbl$z = NA
    tbl$resampling_name_z = NA
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


chunk_size = 200L

i = seq_len(nrow(tbl2))

chunks = data.table(id = seq_len(nrow(tbl2)), chunk = batchtools::chunk(seq_len(nrow(tbl2)), chunk.size = chunk_size, shuffle = TRUE))

chunked_args = map(unique(chunks$chunk), function(cid) {
  ids = chunks[list(cid), "id", on = "chunk"]$id
  map(ids, function(i) {
    list(
      name = tbl2[i, "name"][[1]],
      inference = tbl2[i, "inference"][[1]],
      x = tbl2[i, "x"][[1]],
      y = tbl2[i, "y"][[1]],
      z = tbl2[i, "z"][[1]],
      args = tbl2[i, "args"][[1]][[1]],
      learner_name = tbl2[i, "learner_name"][[1]],
      resampling_name = tbl2[i, "resampling_name_x"][[1]],
      task_name = tbl2[i, "task_name"][[1]],
      size = tbl2[i, "size"][[1]],
      repl = tbl2[i, "repl"][[1]]
    )
  })
})


batchMap(..args = chunked_args, fun = function(..args) {
  map(..args, function(arg) {
    inference = getFromNamespace(arg$inference, ns = "inferGE")

    calculate_ci(
      name = arg$name,
      inference = inference,
      x = arg$x,
      y = arg$y,
      z = arg$z,
      args = arg$args,
      learner_name = arg$learner_name,
      task_name = arg$task_name,
      resampling_name = arg$resampling_name,
      size = arg$size,
      repl = arg$repl
    )
  }) |> rbindlist(fill = TRUE)
})

