test_that("infer_ts_boot works", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("nested_bootstrap", reps_outer = 20, reps_inner = 10)

  rr = resample(task, learner, resampling)
  rr_boot = resample(task, learner, rsmp("bootstrap", repeats = 10))
  rr_in = resample(task, learner, rsmp("insample"))

  expect_ci_method(infer_ts_boot, rr, y = rr_boot, z = rr_in, .symmetric = FALSE)

  ci = infer_ts_boot(rr, rr_boot, rr_in)
})

