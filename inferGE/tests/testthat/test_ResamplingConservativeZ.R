test_that("ResamplingConservativeZ works", {
  J = 2
  M = 3
  ratio = 0.78
  r = rsmp("conservative_z", ratio = ratio, J = J, M = M)
  task = tsk("iris")
  r$instantiate(task)

  expect_class(r$instance$subsampling, "ResamplingSubsampling")
  expect_equal(r$instance$subsampling$param_set$values$repeats, J)
  expect_equal(r$instance$subsampling$param_set$values$ratio, ratio)
  expect_equal(r$iters, J + 2 * M * J)
  expect_list(r$instance$subsamplings_variance, len = M)
  expect_list(r$instance$subsamplings_variance[[1]], len = J, types = "ResamplingSubsampling")
  expect_list(r$instance$subsamplings_variance[[2]], len = J, types = "ResamplingSubsampling")
  expect_list(r$instance$subsamplings_variance[[3]], len = J, types = "ResamplingSubsampling")
  expect_equal(r$instance$subsamplings_variance[[1]][[1]]$param_set$values$repeats, 2)
  expect_equal(r$instance$subsamplings_variance[[1]][[2]]$param_set$values$repeats, 2)

  expect_equal(
    length(r$instance$subsamplings_variance[[1]][[1]]$test_set(1)),
    length(r$instance$subsamplings_variance[[1]][[2]]$test_set(1))
  )
  expect_equal(
    length(r$instance$subsamplings_variance[[2]][[1]]$test_set(1)),
    length(r$instance$subsamplings_variance[[2]][[2]]$test_set(1))
  )
  expect_equal(
    length(r$instance$subsamplings_variance[[3]][[1]]$test_set(1)),
    length(r$instance$subsamplings_variance[[3]][[2]]$test_set(1))
  )

  expect_equal(
    length(r$test_set(3)),
    length(r$test_set(4))
  )
  expect_equal(
    length(r$test_set(4)),
    length(r$test_set(5))
  )


  rr = resample(
    task,
    lrn("classif.rpart"),
    r
  )
  expect_class(rr, "ResampleResult")
})
