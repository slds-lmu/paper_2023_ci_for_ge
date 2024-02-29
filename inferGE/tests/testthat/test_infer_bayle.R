test_that("cv", {
  learner = lrn("classif.rpart")
  task = tsk("penguins")
  resampling = rsmp("cv", folds = 10)

  rr = resample(task, learner, resampling)

  tbl = infer_bayle(rr, variance = "all-pairs")
  tbl1 = infer_bayle(rr, variance = "within-fold")

  expect_data_table(tbl, nrow = 1, ncol = 4)
  expect_set_equal(
    colnames(tbl),
    c("estimate", "lower", "upper", "info")
  )
  expect_ci_method(infer_bayle, rr, variance = "within-fold")
  expect_ci_method(infer_bayle, rr, variance = "all-pairs")
})

test_that("repeated cv", {
  learner = lrn("classif.rpart")
  task = tsk("penguins")
  resampling = rsmp("repeated_cv", folds = 3, repeats = 2)

  rr = resample(task, learner, resampling)
  expect_ci_method(infer_bayle, rr, variance = "within-fold")
  expect_ci_method(infer_bayle, rr, variance = "all-pairs")
})

test_that("loo", {
  learner = lrn("regr.rpart")
  task = tsk("mtcars")
  resampling = rsmp("loo")

  rr = resample(task, learner, resampling)
  expect_error(infer_bayle(rr, variance = "within-fold"))
  expect_ci_method(infer_bayle, rr, variance = "all-pairs")
})
