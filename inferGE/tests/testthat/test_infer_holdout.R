test_that("infer_holdout works", {
  rr = resample(
    tsk("iris"),
    lrn("classif.featureless"),
    rsmp("holdout")
  )

  expect_ci_method(infer_holdout, rr)
})
