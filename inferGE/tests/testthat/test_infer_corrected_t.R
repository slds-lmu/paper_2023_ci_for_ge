test_that("infer_corrected_t works", {
  resampling = rsmp("subsampling")
  learner = lrn("regr.rpart")
  task = tsk("boston_housing")

  rr = resample(task, learner, resampling)

  mse = unname(rr$aggregate(msr("regr.mse")))

  result = infer_corrected_t(rr, loss = "se")

  expect_equal(mse, result$estimate)
  expect_equal(result$upper- result$estimate, result$estimate - result$lower)
})
