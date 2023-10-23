test_that("infer_austern_zhou works", {
  resampling = rsmp("austern_zhou", folds = 3)
  task = tsk("iris")
  learner = lrn("classif.featureless")

  rr = resample(
    task, learner, resampling
  )

  res  = infer_austern_zhou(rr)

  expect_data_table(res)
  expect_set_equal(names(res), c("estimate", "lower", "upper", "info"))
  expect_true(res$estimate < res$upper)
  expect_true(res$estimate > res$lower)
})
