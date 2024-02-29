test_that("NestedBootstrap", {
  task = tsk("iris")
  resampling =  rsmp("nested_bootstrap", reps_outer = 2, reps_inner = 3)
  expect_true(resampling$iters == 2 * 4)
  expect_equal(resampling$unflatten(1 + 2), list(rep_outer = 1, rep_inner = 1))
  expect_equal(resampling$unflatten(2 + 2), list(rep_outer = 1, rep_inner = 2))
  expect_equal(resampling$unflatten(3 + 2), list(rep_outer = 1, rep_inner = 3))
  expect_equal(resampling$unflatten(4 + 2), list(rep_outer = 2, rep_inner = 1))
  expect_equal(resampling$unflatten(5 + 2), list(rep_outer = 2, rep_inner = 2))
  expect_equal(resampling$unflatten(6 + 2), list(rep_outer = 2, rep_inner = 3))

  resampling$instantiate(task)
  walk(seq_len(resampling$iters), function(iter) {
    print(iter)
    resampling$train_set(iter)
  })

  task$col_roles$strata = "Species"

  rr = resample(
    tsk("iris"),
    lrn("classif.featureless"),
    resampling
  )
  expect_class(rr, "ResampleResult")

  resampling$instantiate(task)

  inst = resampling$instance
  expect_true(length(inst) == 2)
  expect_equal(inst$insample[[1]]$iters, 1)
  expect_equal(length(inst$bootstrap), 2)
  expect_equal(inst$bootstrap[[1]]$iters, 3)
  expect_equal(inst$bootstrap[[2]]$iters, 3)
})
