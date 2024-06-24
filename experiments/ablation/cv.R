library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)

source(here("experiments", "ablation", "helper.R"))

reg = makeRegistry(
  file.dir = Sys.getenv("ABLATION_CV"),
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr", "data.table", "batchtools")    
)

TBL = make_tbl(c("cv_5", "cv_10", "cv_25", "cv_50", "cv_75", "cv_100"))
GROUPS = unique(TBL$group)

batchExport(list(TBL = TBL))

f = function(.row) {
  tbl = TBL[.row, ]

  reg_path = tbl$reg_path[[1L]]
  reg = loadRegistry(reg_path, make.default = FALSE)
  task = make_task(
    data_id = tbl$data_id[[1L]],
    size = tbl$size[[1L]],
    repl = tbl$repl[[1L]],
    # anything that does not trigger the extra holdout rows
    resampling = rsmp("subsampling") 
  )

  learner = lrn(tbl$learner[[1L]])

  folds = switch(tbl$resampling_name[[1L]],
    "cv_5" = 5,
    "cv_10" = 10,
    "cv_25" = 25,
    "cv_50" = 50,
    "cv_75" = 75,
    "cv_100" = 100,
    stop()
  )
  

  res = loadResult(tbl$job.id[[1L]], reg = reg) 
  predictions = res$test_predictions

  preds = map(predictions, function(pred) list(test = pred))

  resampling = rsmp("cv", folds = folds)
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

  cbind(infer_bayle(rr, alpha = 0.05), data.table(folds = folds))
}

batchMap(.row = 1:nrow(TBL), fun = f)
ids = which(TBL$size == 500)
