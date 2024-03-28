test_that("infer_ts_boot works", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("nested_bootstrap", reps_outer = 20, reps_inner = 2)

  rr = resample(task, learner, resampling)

  expect_ci_method(infer_ts_boot, rr, .symmetric = FALSE)
})

