test_that("infer_52cv works", {
  task = tsk("sonar")
  resampling = rsmp("repeated_cv", folds = 2, repeats = 5)
  learner = lrn("classif.rpart")

  rr = resample(task, learner, resampling)
  infer_52cv(rr)

  expect_ci_method(infer_52cv, rr)
})
