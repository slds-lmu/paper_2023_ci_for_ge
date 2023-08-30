test_that("ResamplingConservativeZ works", {
  r = rsmp("conservative_z", ratio = 0.8, J = 2, M = 3)
  task = tsk("iris")
  r$instantiate(task)

  rr = resample(
    task,
    lrn("classif.rpart"),
    r
  )
  expect_class(rr, "ResampleResult")
})
