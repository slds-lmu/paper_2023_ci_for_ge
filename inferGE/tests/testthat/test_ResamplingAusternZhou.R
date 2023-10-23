test_that("ResamplingAusternZhou works", {
  # uneven number of rows
  task = as_task_regr(data.table(
    x = rnorm(9),
    y = rnorm(9)
  ), target = "y", id = "test")

  resampling = rsmp("austern_zhou", folds = 2)

  expect_true(is.na(resampling$iters))
  resampling$task_nrow = task$nrow
  expect_equal(resampling$iters, (9 - 1) / 2 * 2 + 2 * 2)

  resampling$instantiate(task)
  instance = resampling$instance

  expect_equal(
    length(instance$replace_ids),
    nrow(instance$tbl)
  )

  folds = unlist(lapply(seq_len(resampling$iters), function(i) mlr3misc::get_private(resampling)$.get_fold(i)))
  expect_equal(
    folds,
    rep(c(1, 2), 6)
  )

  ii_replace = unlist(lapply(seq(resampling$iters - 2 * resampling$param_set$values$folds), function(i) mlr3misc::get_private(resampling)$.get_replace(i)))

  expect_equal(
    ii_replace,
    rep(1:4, each = 2)
  )

  resampling$train_set(1)
  resampling$train_set(1)
  resampling$train_set(3)
  train_sets = lapply(seq_len(resampling$iters), function(i) resampling$train_set(i))
  test_sets = lapply(seq_len(resampling$iters), function(i) resampling$test_set(i))

  expect_true(all(map_lgl(lengths(train_sets), function(x) x %in% c(2, 4, 5))))
  expect_true(all(map_lgl(lengths(test_sets), function(x) x %in% c(2, 4, 5))))

  expect_true(all(map_lgl(lengths(train_sets[-(1:2)]), function(x) x == 2)))
  expect_true(all(map_lgl(lengths(test_sets[-(1:2)]), function(x) x == 2)))


  # in this case train and test are completely symmetrical
  tbl_train = train_sets[-(1:2)] |> unlist() |> table()
  tbl_test = test_sets[-(1:2)] |> unlist() |> table()
  expect_equal(tbl_train, tbl_test)

  # the ignored_id (due to uneven nrow) is actually ignored
  expect_false(
    resampling$instance$ignored_id %in% unique(unlist(train_sets[-(1:2)]))
  )
  expect_true(
    resampling$instance$ignored_id %in% unique(unlist(train_sets[1:2]))
  )

  expect_false(
    resampling$instance$ignored_id %in% unique(unlist(test_sets[-(1:2)]))
  )

  expect_true(
    resampling$instance$ignored_id %in% unique(unlist(test_sets[1:2]))
  )

  expect_equal(
    Reduce(function(...) sum(lengths(...)), train_sets),
    Reduce(function(...) sum(lengths(...)), test_sets)
  )


  rr = resample(
    task,
    lrn("regr.featureless"),
    resampling
  )
  expect_class(rr, "ResampleResult")

  p = as.data.table(rr$prediction())

  expect_equal(
    nrow(p),
    task$nrow + (task$nrow %/% 2) * (task$nrow %/% 2 + 1)
  )
})
