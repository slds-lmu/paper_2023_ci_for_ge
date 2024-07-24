library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)

source(here("experiments", "ablation", "helper.R"))

reg = makeRegistry(
  file.dir = Sys.getenv("ABLATION_NCV_CHEAP"),
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr", "batchtools", "data.table")
)

reg_path = "/gscratch/sfische6/benchmarks/ci_for_ge/final_resample_more"
exp_reg = loadRegistry(reg_path, make.default = FALSE)
tbl = unwrap(getJobTable(reg = exp_reg))
tbl = tbl[resampling_name %in% "nested_cv", c("job.id", "repl", "size", "task_name", "learner_id", "data_id", "resampling_name")]
tbl$reg_path = reg_path
tbl[, let(group = .GRP), by = c("size", "task_name", "learner_id")]
setnames(tbl, c("task_name", "learner_id"), c("dgp", "learner"))

TBL = tbl
GROUPS = unique(TBL$group)

f = function(.row, reg) {
  tbl = TBL[.row, ]

  task = make_task(
    data_id = tbl$data_id[[1L]],
    size = tbl$size[[1L]],
    repl = tbl$repl[[1L]],
    # anything that does not trigger the extra holdout rows
    resampling = rsmp("subsampling") 
  )

  learner = lrn(tbl$learner[[1L]])

  repeats = 10
  folds = 5

  rbindlist(map(1:10, function(r) {
    res = loadResult(tbl$job.id[[1L]], reg) 
    predictions = res$test_predictions

    preds = map(predictions[1:(folds^2 * r)], function(pred) list(test = pred))

    resampling = rsmp("nested_cv", repeats = r, folds = folds)
    resampling$instantiate(task)

    data = as_result_data(
     task = task,
     learners = lapply(seq_len(resampling$iters), function(i) learner),
     predictions = preds,
     resampling = resampling,
     iterations = seq_len(resampling$iters),
     learner_states = NULL,
     store_backends = TRUE)

    rr = ResampleResult$new(data)

    cbind(infer_bates(rr, alpha = 0.05), data.table(reps_outer = r))
  }), fill = TRUE)
}

ids = 1:nrow(TBL)
splits = split(ids, ceiling(seq_along(ids) / 100))
g = function(ii) {
  reg = loadRegistry(REG_PATH, make.default = FALSE)
  rbindlist(map(ii, f, reg = reg))
}
batchExport(list(TBL = TBL, f = f, REG_PATH = reg_path))

batchMap(ii = splits, fun = g)
