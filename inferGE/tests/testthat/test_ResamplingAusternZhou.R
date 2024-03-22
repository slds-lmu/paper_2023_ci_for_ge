test_that("ResamplingAusternZhou works", {
  # uneven number of rows
  task = as_task_regr(data.table(
    x = rnorm(9),
    y = rnorm(9)
  ), target = "y", id = "test")

  resampling = rsmp("austern_zhou", folds = 2)
  resampling$instantiate(task)

  expect_true(length(resampling$instance$left_half) == 4)
  expect_equal(resampling$iters, 2 * (9 %/% 2 + 1))

  expect_permutation(
    c(resampling$train_set(1), resampling$train_set(2)),
    resampling$instance$left_half
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

  task = tsk("mtcars")$filter(1:10)
  resampling = rsmp("austern_zhou", folds = 2)$instantiate(task)
  expect_true(length(resampling$instance$left_half) == 5)

  expect_permutation(
    resampling$instance$left_half,
    c(resampling$train_set(1), resampling$train_set(2))
  )

  expect_permutation(
    resampling$instance$left_half,
    c(resampling$test_set(1), resampling$test_set(2))
  )

  expect_permutation(
    unique(unlist(lapply(seq_len(resampling$iters)[3:resampling$iters], FUN = function(i) resampling$train_set(i)))),
    task$row_ids
  )
})

test_that("with repeats", {
  task = as_task_regr(data.table(
    x = rnorm(10),
    y = rnorm(10)
  ), target = "y", id = "test")


  resampling = rsmp("austern_zhou", folds = 2, repeats = 3)
  resampling$instantiate(task)

  first_train = unique(unlist(lapply(1:6, FUN = function(i) resampling$train_set(i))))
  first_test = unique(unlist(lapply(1:6, FUN = function(i) resampling$test_set(i))))

  expect_permutation(
    unique(first_train),
    resampling$instance$left_half
  )
  expect_permutation(
    unique(first_test),
    resampling$instance$left_half
  )

  rep1 = 7:12
  rep2 = 13:18
  rep3 = 19:24
  rep4 = 25:30
  rep5 = 31:36

  test = function(is) {
    train_ids = unlist(lapply(is, function(i) resampling$train_set(i)))
    test_ids = unlist(lapply(is, function(i) resampling$test_set(i)))
    expect_equal(
      unique(table(train_ids)),
      3
    )
    expect_equal(
      unique(table(test_ids)),
      3
    )
    expect_equal(
      unique(length(setdiff(test_ids, resampling$instance$left_half))), 1
    )
    expect_equal(
      unique(length(setdiff(train_ids, resampling$instance$left_half))), 1
    )
    for (i in is) {
      expect_disjunct(resampling$test_set(i), resampling$train_set(i))
    }
  }

  test(rep1)
  test(rep2)
  test(rep3)
  test(rep4)
  test(rep5)
})
