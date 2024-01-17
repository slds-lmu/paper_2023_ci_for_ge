test_that("losses", {
  task = tsk("mtcars")
  p = lrn("regr.rpart")$train(task)$predict(task)
  expect_numeric(percentual_se(p$truth, p$response))
  expect_numeric(standardized_se(p$truth, p$response))
})
