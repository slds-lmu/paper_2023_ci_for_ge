test_that("cv", {
  learner = lrn("classif.rpart")
  task = tsk("penguins")
  resampling = rsmp("cv", folds = 10)

  rr = resample(task, learner, resampling)

  tbl = infer_bayle(rr, variance = "all-pairs")
  tbl1 = infer_bayle(rr, variance = "within-fold")

  expect_true(tbl1$lower != tbl$lower)
  expect_true(tbl1$upper != tbl$upper)
  expect_true(tbl1$estimate == tbl$estimate)

  expect_data_table(tbl, nrow = 1, ncol = 4)
  expect_set_equal(
    colnames(tbl),
    c("estimate", "lower", "upper", "info")
  )
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
