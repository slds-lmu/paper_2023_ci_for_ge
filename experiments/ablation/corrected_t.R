library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)

source(here("experiments", "ablation", "helper.R"))

reg = makeExperimentRegistry(
  file.dir = Sys.getenv("ABLATION_CORT"),
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr")
)

TBL = make_tbl("corrected_t_100")
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

  learner = lrn(tbl$learner[[1L]])

  ratio = 0.9

  rbindlist(map(c(5, seq(10, 100, by = 10))), function(r) {
    res = loadResult(tbl$job.id[[1L]], reg) 
    predictions = res$test_predictions

    preds = map(predictions[1:r], function(pred) list(test = pred))

    resampling = rsmp("corrected_t", repeats = r, ratio = ratio)
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

    cbind(infer_corrected_t(rr, alpha = 0.05), data.table(reps = r))
  }), fill = TRUE)
}


f(1)
# batchMap(.row = TBL$job.id)

# ids = TBL[learner_id == "linear" & size == 500L]
