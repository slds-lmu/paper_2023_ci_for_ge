test_that("infer_corrected_t works", {
  resampling = rsmp("subsampling", repeats = 10)
  learner = lrn("regr.rpart")
  task = tsk("boston_housing")

  rr = resample(task, learner, resampling)

  expect_ci_method(infer_corrected_t, rr)
})
