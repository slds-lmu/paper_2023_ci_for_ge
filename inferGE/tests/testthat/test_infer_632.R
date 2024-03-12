test_that("infer_632plus", {
  library(mlr3pipelines)
  learner = PipeOpMetaRobustify$new() %>>% lrn("classif.rpart") |> as_learner()
  rr = resample(tsk("iris"), learner, rsmp("bootstrap"))
  rr_insample = resample(tsk("iris"), learner, rsmp("insample"))
  infer_632plus(rr, rr_insample)

  expect_ci_method(infer_632plus, rr, y = rr_insample)
})
