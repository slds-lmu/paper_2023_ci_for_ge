test_that("simple ci works", {
  learner = lrn("classif.rpart")
  task = tsk("iris")
  resampling = rsmp("cv", folds = 10)

  rr = resample(task, learner, resampling)

  infer_simple(rr)


})