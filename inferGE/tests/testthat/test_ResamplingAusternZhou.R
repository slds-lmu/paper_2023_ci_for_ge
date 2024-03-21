test_that("ResamplingAusternZhou works", {
  # uneven number of rows
  task = as_task_regr(data.table(
    x = rnorm(9),
    y = rnorm(9)
  ), target = "y", id = "test")

  resampling = rsmp("austern_zhou", folds = 2)

  resampling$instantiate(task)
  expect_equal(resampling$iters, 2 * (9 %/% 2 + 2))

  expect_equal(
    sum(lengths(list(resampling$train_set(1), resampling$train_set(2)))),
    task$nrow
  )
  expect_permutation(
    c(resampling$train_set(1), resampling$train_set(2)),
    task$row_ids
  )
  expect_permutation(
    c(resampling$test_set(1), resampling$test_set(2)),
    task$row_ids
  )

  expect_equal(
    sum(lengths(list(resampling$train_set(3), resampling$train_set(4)))),
    task$nrow %/% 2
  )
  expect_equal(
    sum(lengths(list(resampling$test_set(3), resampling$test_set(4)))),
    task$nrow %/% 2
  )

  expect_equal(
    sum(lengths(list(resampling$test_set(3), resampling$test_set(4)))),
    task$nrow %/% 2
  )
  for (i in seq_len(resampling$iters)) {
    expect_disjunct(resampling$test_set(i), resampling$train_set(i))
  }
})
