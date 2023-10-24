test_that("infer_bates works for ResampleResult", {
  res = rsmp("nested_cv", folds = 10)
  learner = lrn("classif.rpart")
  task = tsk("sonar")

  rr = resample(task, learner, res)

  expect_ci_method(infer_bates, rr)
})
