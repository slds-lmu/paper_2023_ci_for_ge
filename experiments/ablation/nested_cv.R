library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)

source(here("experiments", "ablation", "helper.R"))

# 1. We merge all the registries
# 2. 

if (FALSE) {
  reg = makeExperimentRegistry(
    file.dir = Sys.getenv("ABLATION_NCV"),
    packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3db", "inferGE", "mlr3oml", "mlr3misc", "here", "duckdb", "DBI", "lgr")
  )

  tbl = make_tbl("nested_cv")
  GORUPS = unique(tbl$group)

}

exportBatch(list(TBL = tbl))

f = function(.group) {
  tbl = TBL[group == .group, ]
  reg_path = tbl$reg_path[1L]
  reg = loadRegistry(reg_path)

  map(1:nrow(tbl), function(i) {
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
 })
}


f(1)
