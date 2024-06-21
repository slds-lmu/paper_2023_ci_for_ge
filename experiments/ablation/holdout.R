library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)

source(here("experiments", "ablation", "helper.R"))

reg = makeRegistry(
  file.dir = Sys.getenv("ABLATION_HO"),
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr", "data.table", "batchtools")    
)

TBL = make_tbl(paste0("holdout_", c("50", "60", "66", "75", "80", "90")))
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

  ratio = switch(tbl$resampling_name[[1L]],
    "holdout_50", = 0.5,
    "holdout_60", = 0.6,
    "holdout_66", = 2/3,
    "holdout_70", = 0.7,
    "holdout_80", = 0.8,
    "holdout_90", = 0.9,
    stop()
  )
  

  res = loadResult(tbl$job.id[[1L]], reg = reg) 
  predictions = res$test_predictions

  preds = map(predictions, function(pred) list(test = pred))

  resampling = rsmp("holdout", ratio = ratio)
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

  cbind(infer_holdout(rr, alpha = 0.05), data.table(ratio = ratio))
}

batchMap(.row = 1:nrow(TBL), fun = f)
ids = which(TBL$size == 500)

