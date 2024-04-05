test_that("percentual_se", {
  task = tsk("mtcars")
  p = lrn("regr.rpart")$train(task)$predict(task)
  expect_numeric(percentual_se(p$truth, p$response, task = task))
  expect_equal(length(percentual_se(p$truth, p$response, task = task)), 32)
  expect_numeric(standardized_se(p$truth, p$response, task = task))
  expect_equal(length(standardized_se(p$truth, p$response, task = task)), 32)
})

test_that("standardized_se", {
  task = tsk("mtcars")
  p = lrn("regr.rpart")$train(task)$predict(task)
  expect_numeric(percentual_se(p$truth, p$response))
  expect_equal(length(percentual_se(p$truth, p$response)), 32)
  expect_numeric(standardized_se(p$truth, p$response, task = task))
  expect_equal(length(standardized_se(p$truth, p$response, task = task)), 32)

  rr = resample(task, lrn("regr.rpart"), rsmp("holdout"))

  ci = infer_holdout(rr, loss_fn = list("std_se" = standardized_se))

})


test_that("logloss", {
  pred = lrn("classif.rpart", predict_type = "prob")$train(tsk("iris"))$predict(tsk("iris"))
  ll = logloss(pred$truth, pred$prob)
  expect_numeric(ll)
  expect_equal(length(ll), 150)
})

test_that("bbrier", {
  rr = resample(tsk("sonar"), lrn("classif.rpart", predict_type = "prob"), rsmp("insample"))
  pred = rr$predictions("test")[[1L]]
  loss = bbrier(pred$truth, pred$prob)
  expect_numeric(loss)
  expect_equal(length(loss), 208)

  measure = msr("classif.bbrier")
  expect_equal(unname(pred$score(measure)), mean(loss))
})

test_that("measures", {
  std_se = msr("regr.std_mse")
  percentual_se = msr("regr.percentual_mse")

  task = tsk("mtcars")
  pred = lrn("regr.rpart")$train(task)$predict(task)

  expect_numeric(pred$score(std_se, task = task), len = 1L)
  expect_numeric(pred$score(percentual_se, task = task), len = 1L)
})

test_that("est_gamma with standardized_se", {
  task = tsk("mtcars")
  p = lrn("regr.rpart")$train(task)$predict(task)
  expect_numeric(est_gamma(p, standardized_se, task = task, train_set = task$row_ids))
})
