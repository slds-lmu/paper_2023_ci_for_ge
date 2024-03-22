test_that("infer_austern_zhou works", {
  withr::local_seed(1)
  resampling = rsmp("austern_zhou", folds = 3)
  task = tsk("iris")
  task$row_roles$use = c(1, 2, 3, 51, 52, 53, 101, 102, 103, 104, 105, 106)
  learner = lrn("classif.featureless")

  rr = resample(
    task, learner, resampling
  )
  rr_cv = resample(
    task, learner, rsmp("cv", folds = 3L)
  )

  expect_ci_method(infer_austern_zhou, rr, y = rr_cv)
})

test_that("infer_austern_zhou works with repeats", {
  withr::local_seed(1)
  resampling = rsmp("austern_zhou", folds = 3, repeats = 10)
  task = tsk("iris")
  task$row_roles$use = c(1, 2, 3, 51, 52, 53, 101, 102, 103, 104, 105, 106)
  learner = lrn("classif.featureless")

  rr = resample(
    task, learner, resampling
  )
  rr_cv = resample(
    task, learner, rsmp("repeated_cv", folds = 3L, repeats = 10)
  )

  expect_ci_method(infer_austern_zhou, rr, y = rr_cv)
})
