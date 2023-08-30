test_that("infer_bates works for ResampleResult", {
  res = rsmp("nested_cv", folds = 5)
  learner = lrn("classif.rpart")
  task = tsk("penguins")

  rr = resample(task, learner, res)

  tbl = infer_bates(rr, alpha = 0.05, loss = "zero_one")
  expect_data_table(tbl)
  expect_true(tbl$estimate > tbl$lower)
  expect_true(tbl$estimate < tbl$upper)
  expect_true(tbl$lower < tbl$upper)
  expect_set_equal(
    names(tbl),
    c("estimate", "lower", "upper", "bias", "mse", "err_ncv", "err_cv")
  )
})

test_that("infer_bates works for BenchmarkResult", {
  res = rsmp("nested_cv", folds = 5)
  learner = lrn("classif.rpart")
  tasks = tsks(c("penguins", "iris"))

  design = benchmark_grid(tasks, learner, res)
  bmr = benchmark(design)

  tbl = infer_bates(bmr, loss = "zero_one")

  expect_data_table(tbl, nrows = 2)
  expect_set_equal(
    names(tbl),
    c("estimate", "lower", "upper", "bias", "mse", "err_ncv", "err_cv", "learner_id", "task_id", "resampling_id",
      "resample_result"
    )
  )
})
