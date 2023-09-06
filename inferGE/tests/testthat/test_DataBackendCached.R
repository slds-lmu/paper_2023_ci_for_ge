test_that("DataBackendCached works", {
  backend = as_data_backend(data.table::data.table(x = rnorm(150), y = rnorm(150)))

  backend_cached = DataBackendCached$new(
    backend,
    ids = 1:140
  )

  expect_true(is.null(mlr3misc::get_private(backend_cached)$.cache))

  task = as_task_regr(backend, target = "y")
  task_cached = as_task_regr(backend_cached, target = "y")
  task$row_roles$use = 1:140

  expect_true(is.null(mlr3misc::get_private(task_cached$backend)$.cache))

  task_cached$backend$do_caching = TRUE

  expect_equal(backend$data(10, c("x", "y")), backend_cached$data(10, c("x", "y")))
  expect_equal(task$row_ids, task_cached$row_ids)
  expect_equal(task$data(), task_cached$data())
  expect_equal(task$missings(), task_cached$missings())
  expect_equal(task$backend$distinct(2:12, "x"), task_cached$backend$distinct(2:12, "x"))

  # the rownamesof the backend and the cached backend are not equal
  # because the latter is a view on a subset of the former
  expect_equal(task$row_ids, task_cached$backend$rownames)
  expect_equal(task$backend$colnames, task_cached$backend$colnames)
  expect_equal(task$nrow, task_cached$backend$nrow)

  learner = lrn("regr.rpart")
  rr = resample(
    task_cached,
    learner,
    rsmp("cv", folds = 3)

  )

  expect_class(rr, "ResampleResult")
})


test_that("robustify throws no error", {
  task = tsk("penguins")
  library(mlr3verse)
  library(mlr3db)
  backend = as_duckdb_backend("/gscratch/sfische6/ci_for_ge_data/simulated_physiochemical_protein.parquet")

  backend_cached = DataBackendCached$new(
    backend,
    ids = 1:1000
  )

  task = as_task_regr(backend_cached, target = "RMSD")
  backend_cached$do_caching = TRUE

  learner = lrn("regr.lm")

  learner = as_learner(ppl("robustify", learner = learner) %>>% learner)

  resample(
    task, 
    learner,
    rsmp("cv")
  )


})