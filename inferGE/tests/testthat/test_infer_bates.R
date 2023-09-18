test_that("infer_bates works for ResampleResult", {
  res = rsmp("nested_cv", folds = 10)
  learner = lrn("classif.rpart")
  task = tsk("sonar")

  rr = resample(task, learner, res)

  tbl = infer_bates(rr, alpha = 0.05)
  expect_data_table(tbl)
  expect_true(tbl$estimate > tbl$lower)
  expect_true(tbl$estimate < tbl$upper)
  expect_true(tbl$lower < tbl$upper)
  expect_set_equal(
    names(tbl),
    c("estimate", "lower", "upper", "info")
  )
})