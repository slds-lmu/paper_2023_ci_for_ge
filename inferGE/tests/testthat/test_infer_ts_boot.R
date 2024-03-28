test_that("two-stage bootstrap works", {
  library(tictoc)
  library(mlr3pipelines)
  learner = as_learner(PipeOpMetaRobustify$new() %>>% lrn("classif.featureless", predict_type = "prob"))
  rr = resample(tsk("sonar"), learner, rsmp("nested_bootstrap", reps_outer = 10, reps_inner = 3))
  expect_ci_method(infer_ts_boot, rr, .symmetric = FALSE)
  expect_ci_method(infer_ts_boot, rr, .symmetric = FALSE, loss_fn = list(bbrier = bbrier))
})
