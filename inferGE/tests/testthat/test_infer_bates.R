test_that("infer_bates works for ResampleResult", {
  res = rsmp("nested_cv", folds = 5, repeats = 2)
  learner = lrn("classif.rpart")
  task = tsk("sonar")

  rr = resample(task, learner, res)
  res = infer_bates(rr, alpha = 0.05)

  expect_ci_method(infer_bates, rr)
})
