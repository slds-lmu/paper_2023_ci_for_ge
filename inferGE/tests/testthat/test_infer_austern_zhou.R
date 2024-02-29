test_that("infer_austern_zhou works", {
  withr::local_seed(1)
  resampling = rsmp("austern_zhou", folds = 3)
  task = tsk("iris")
  task$row_roles$use = c(1, 2, 3, 51, 52, 53, 101, 102, 103, 104, 105, 106)
  learner = lrn("classif.featureless")

  rr = resample(
    task, learner, resampling
  )

  expect_ci_method(infer_austern_zhou, rr)

  res  = infer_austern_zhou(rr)

  expect_data_table(res)
  expect_set_equal(names(res), c("estimate", "lower", "upper", "info"))
  expect_true(res$estimate < res$upper)
  expect_true(res$estimate > res$lower)
})
