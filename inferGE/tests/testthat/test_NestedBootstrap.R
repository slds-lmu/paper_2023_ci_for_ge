test_that("NestedBootstrap", {
  resampling =  rsmp("nested_bootstrap", reps_outer = 2, reps_inner = 3, ratio = 0.8)
  expect_true(resampling$iters == 2 * 3)
  expect_equal(resampling$unflatten(1), list(rep_outer = 1, rep_inner = 1))
  expect_equal(resampling$unflatten(2), list(rep_outer = 1, rep_inner = 2))
  expect_equal(resampling$unflatten(3), list(rep_outer = 1, rep_inner = 3))
  task$col_roles$strata = "Species"

  rr = resample(
    tsk("iris"),
    lrn("classif.featureless"),
    resampling
  )
  expect_class(rr, "ResampleResult")

  resampling$instantiate(task)

  inst =resampling$instance
  expect_true(length(inst) == 2)
  expect_equal(inst[[1]]$iters, 3)
  expect_equal(inst[[1]]$param_set$values$ratio, 0.8)
  expect_equal(inst[[1]]$param_set$values$repeats, 3)
  expect_equal(inst[[2]]$iters, 3)
  expect_equal(inst[[2]]$param_set$values$ratio, 0.8)
  expect_equal(inst[[2]]$param_set$values$repeats, 3)
})
