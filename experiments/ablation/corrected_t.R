library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)

# reg = loadRegistry(Sys.getenv("RESAMPLE_PATH"))

# tbl = unwrap(getJobTable())
# tbl = tbl[resampling_id == "corrected_t_100", "job.id"][[1L]]

repeats = 100L
ratio = 0.9

learner = lrn("regr.rpart")
task = tsk("mtcars")
resampling = rsmp("corrected_t", repeats = repeats, ratio = ratio)

rr = resample(task, learner, resampling)
predictions = rr$predictions()

cis = rbindlist(map(1:repeats, function(r) {
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

  infer_corrected_t(rr, alpha = 0.05)
}), fill = TRUE)
