library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)

source(here("experiments", "ablation", "helper.R"))

reg = makeExperimentRegistry(
  file.dir = Sys.getenv("ABLATION_NCV"),
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr")
)

TBL = make_tbl("nested_cv")
GROUPS = unique(tbl$group)

exportBatch(list(TBL = TBL))

f = function(.row) {
  tbl = TBL[.row, ]

  reg_path = tbl$reg_path[[1L]]
  reg = loadRegistry(reg_path)
  task = make_task(
    data_id = tbl$data_id[[1L]],
    size = tbl$size[[1L]],
    repl = tbl$repl[[1L]],
    # anything that does not trigger the extra holdout rows
    resampling = rsmp("subsampling") 
  )

  learner = lrn(tbl$learner_id[[1L]])

  repeats = 200
  folds = 5

  rbindlist(map(1:repeats, function(r) {
    res = loadResult(reg) 
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

    infer_bates(rr, alpha = 0.05)
  }), fill = TRUE)
}


f(1)
# batchMap(.row = TBL$job.id)

# ids = TBL[learner_id == "linear" & size == 500L]
