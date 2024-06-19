library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)

source(here("experiments", "ablation", "helper.R"))

reg = makeExperimentRegistry(
  file.dir = Sys.getenv("ABLATION_CONZ"),
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr")
)

TBL = make_tbl("conservative_z")
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

  reps = c(5, 50, by = 5)

  rbindlist(map(reps, function(outer_reps) {
    rbindlist(map(reps, function(inner_reps) {
      res = loadResult(tbl$job.id[[1L]], reg) 
      predictions = res$test_predictions

      preds = map(predictions[1:r], function(pred) list(test = pred))

      resampling = rsmp("conservative_z", M = outer_reps, J = inner_reps)
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

      cbind(infer_conservative_z(rr, alpha = 0.05), 
        data.table(outer_reps = outer_reps, inner_reps = inner_reps))
    }), fill = TRUE)
  }), fill = TRUE)
}


f(1)
# batchMap(.row = TBL$job.id)

# ids = TBL[learner_id == "linear" & size == 500L]
