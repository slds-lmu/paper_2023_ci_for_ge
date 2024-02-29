test_that("losses", {
  task = tsk("mtcars")
  p = lrn("regr.rpart")$train(task)$predict(task)
  expect_numeric(percentual_se(p$truth, p$response))
  expect_equal(length(percentual_se(p$truth, p$response)), 32)
  expect_numeric(standardized_se(p$truth, p$response))
  expect_equal(length(standardized_se(p$truth, p$response)), 32)
})

test_that("logloss", {
  pred = lrn("classif.rpart", predict_type = "prob")$train(tsk("iris"))$predict(tsk("iris"))
  ll = logloss(pred$truth, pred$prob)
  expect_numeric(ll)
  expect_equal(length(ll), 150)
})

test_that("bbrier", {
  pred = lrn("classif.rpart", predict_type = "prob")$train(tsk("sonar"))$predict(tsk("sonar"))

  loss = bbrier(pred$truth, pred$prob)


})
