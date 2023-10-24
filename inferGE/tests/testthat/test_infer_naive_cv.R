test_that("infer_naive_cv works", {
  rr = resample(
    tsk("iris"),
    lrn("classif.rpart"),
    rsmp("cv")
  )

  expect_ci_method(infer_naive_cv, rr)
})
