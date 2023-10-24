test_that("infer_conservative_z works", {
  resampling = rsmp("conservative_z", ratio = 0.8, J = 10, M = 3)
  learner = lrn("classif.rpart")
  task = tsk("iris")

  rr = resample(task, learner, resampling)

  expect_ci_method(infer_conservative_z, rr)
})
