test_that("two-stage bootstrap works", {
  library(mlr3pipelines)
  learner = as_learner(PipeOpMetaRobustify$new() %>>% lrn("classif.featureless"))
  rr = resample(tsk("iris"), learner, rsmp("nested_bootstrap", reps_outer = 10, reps_inner = 3))
  expect_ci_method(infer_ts_boot, rr)
})
