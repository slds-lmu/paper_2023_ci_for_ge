test_that("infer_botstrap_cv works", {
  resampling = rsmp("bootstrap_ccv", ratio = 1, repeats = 10)
  task =  tsk("mtcars")
  task$row_roles$use = 1:20

  rr = resample(
    task,
    lrn("regr.rpart"),
    resampling
  )

  expect_ci_method(infer_bootstrap_ccv, rr)

  res = infer_bootstrap_ccv(rr, alpha = 0.05)
  expect_equal(names(res), c("estimate", "lower", "upper"))
  expect_data_table(res)
  expect_true(res$estimate > res$lower)
  expect_true(res$estimate < res$upper)

  res2 = infer_bootstrap_ccv(rr, alpha = 0.3)

  expect_true(res2$lower > res$lower)
  expect_true(res2$upper < res$upper)
})
