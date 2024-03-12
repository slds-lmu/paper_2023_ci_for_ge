test_that("infer_632 works", {
  library(mlr3pipelines)
  learner = PipeOpMetaRobustify$new() %>>% lrn("classif.rpart") |> as_learner()
  rr = resample(tsk("iris"), learner, rsmp("bootstrap", repeats = 10))
  rr$prediction("test")
  ci = infer_oob(rr)

  expect_ci_method(infer_oob, rr)
})
