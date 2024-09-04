library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)

source(here("experiments", "ablation", "helper.R"))

reg = makeRegistry(
  file.dir = Sys.getenv("ABLATION_CONZ2"),
  packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr", "data.table", "batchtools")    
)

TBL = make_tbl("conservative_z_50")
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

  rbindlist(map(10, function(outer_reps) {
    rbindlist(map(5, function(inner_reps) {
      res = loadResult(tbl$job.id[[1L]], reg = reg) 
      predictions = res$test_predictions

      # the first and then the second inner subsampling from a pair
      one_pair = c(1:inner_reps, 50 + 1:inner_reps)
      # the first 50 are the unpaired subsampling and then we add 100 (2 x 50) for each paired subsampling
      inner = unlist(map(1:outer_reps, function(r) 50 + one_pair + 100 * (r - 1)))
      ids = c(1:inner_reps, inner)

      preds = map(predictions[ids], function(pred) list(test = pred))

      resampling = rsmp("conservative_z", M = outer_reps, J = inner_reps, ratio = 0.9)
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

ids = 1:nrow(TBL)
splits = split(ids, ceiling(seq_along(ids) / 20))
g = function(ii) rbindlist(map(ii, f))
batchExport(list(TBL = TBL, f = f))

batchMap(ii = splits, fun = g)
