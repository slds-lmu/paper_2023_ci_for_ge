test_that("DataBackendCached works", {
  backend = as_data_backend(data.table::data.table(x = rnorm(150), y = rnorm(150)))

  backend_cached = DataBackendCached$new(
    backend,
    ids = 1:140
  )

  expect_equal(backend$data(10, c("x", "y")), backend_cached$data(10, c("x", "y")))

  task = as_task_regr(backend, target = "y")
  task$row_roles$use = 1:140
  task_cached = as_task_regr(backend_cached, target = "y")

  expect_equal(task$row_ids, task_cached$row_ids)
  expect_equal(task$data(), task_cached$data())
  expect_equal(task$missings(), task_cached$missings())
  expect_equal(task$backend$distinct(2:12, "x"), task_cached$backend$distinct(2:12, "x"))

  # the rownamesof the backend and the cached backend are not equal
  # because the latter is a view on a subset of the former
  expect_equal(task$row_ids, task_cached$backend$rownames)
  expect_equal(task$backend$colnames, task_cached$backend$colnames)
  expect_equal(task$backend$nrow, task_cached$backend$nrow)
  expect_equal(task$ncol, task_cached$backend$ncol)

  learner = lrn("regr.rpart")
  rr = resample(
    task_cached,
    learner,
    rsmp("cv", folds = 3)

  )

  expect_class(rr, "ResampleResult")
})
