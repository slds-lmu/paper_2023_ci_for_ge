test_that("two-stage bootstrap works", {
  rr = resample(tsk("iris"), lrn("classif.featureless"), rsmp("nested_bootstrap", reps_outer = 10, reps_inner = 3))
  expect_ci_method(infer_ts_boot, rr)
})
