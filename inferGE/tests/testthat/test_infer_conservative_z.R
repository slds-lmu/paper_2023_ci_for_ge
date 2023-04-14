test_that("infer_conservative_z works", {
  resampling = rsmp("subsampling")
  learner = lrn("regr.rpart")
  task = tsk("mtcars")

  rr = resample(task, learner, resampling)

  result = infer_corrected_t(rr, loss = "mse")
  
})
