test_that("infer_botstrap_cv works", {
  resampling = rsmp("bootstrap_ccv", ratio = 1, repeats = 20)
  task =  tsk("mtcars")
  task$row_roles$use = 1:20

  rr = resample(
    task,
    lrn("regr.rpart"),
    resampling
  )

  rr1 = resample(
    task,
    lrn("regr.rpart"),
    rsmp("loo")
  )

  ci1 = infer_bootstrap_ccv(rr, y = rr1)
  ci2 = infer_bootstrap_ccv(rr)
  expect_true(ci1$lower !=  ci2$lower)
  expect_true(ci1$upper !=  ci2$upper)
  expect_true(ci1$estimate !=  ci2$estimate)


  expect_ci_method(infer_bootstrap_ccv, rr, .symmetric = FALSE)
  expect_ci_method(infer_bootstrap_ccv, rr, y = rr1, .symmetric = FALSE)
})
