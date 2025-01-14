library(batchtools)
library(data.table)
library(mlr3misc)
library(inferGE)

source(here::here("experiments", "helper.R"))

EXPERIMENT_PATH = Sys.getenv("RESAMPLE_PATH_MLP")
EVAL_PATH = Sys.getenv("CI_PATH_MLP")

EVAL_REG = if (file.exists(EVAL_PATH)) {
  loadRegistry(EVAL_PATH, writeable = TRUE,  conf.file = here::here("experiments", "mlp", "batchtools.conf.R"))
} else {
  makeRegistry(EVAL_PATH,
	       conf.file = here::here("experiments", "mlp", "batchtools.conf.R"),
    packages = c("inferGE", "mlr3misc", "mlr3", "digest", "withr", "uuid", "batchtools", "tictoc", "duckdb", "mlr3pipelines", "mlr3learners", "ranger", "mlr3oml", "here", "data.table", "mlr3mbo", "mlr3tuning", "mlr3torch"),
    seed = 1L,
  )
}
EXPERIMENT_REG = loadRegistry(EXPERIMENT_PATH, make.default = FALSE)
EXPERIMENT_TBL = unwrap(getJobTable(findDone(reg = EXPERIMENT_REG), reg = EXPERIMENT_REG))

# order is as in experiments/design.R
# we use the default alpha = 0.05 everywhere
# 1: name (unique)
# 2: inference method
# 3: named identifiers for the resampling_named from the experiment's job table
# 4: additional parameters passed to the resampling method
EVAL_CONFIG = list(
  list("corrected_t_25",     "infer_corrected_t",    list(x = "subsampling_25_90"),                             list()),
  list("nested_cv_75",       "infer_bates",          list(x = "nested_cv_75"),                                  list()),
  list("conservative_z_105", "infer_conservative_z", list(x = "conservative_z_105"),                            list())
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
  make_task_highdim = make_task_highdim,
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

