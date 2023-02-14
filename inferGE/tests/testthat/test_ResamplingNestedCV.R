test_that("ResamplingNestedCV works", {
  res = rsmp("nested_cv")
  res
  task = tsk("iris")
  folds = 10
  res$param_set$values$folds = folds
  res$instantiate(task)

  train_sets = lapply(seq_len(res$iters), function(x) res$train_set(x))
  test_sets = lapply(seq_len(res$iters), function(x) res$test_set(x))


  rr = resample(task, lrn("classif.debug"), res)

  expect_equal(res$unflatten(11), list(outer = 1, inner = 1))


  expect_true(length(train_sets) == folds^2)

  # All observations appear in the same number of splits
  # Also each train sample is in (folds - 1)^2 splits, because
  # 1. If it is in the outer test set (1) then it will never bee in one of the train sets.
  # If it is in the outer train set (folds - 1), then it will be in (folds - 1 - 1) inner train sets, because it is
  # once in the inner test set.
  # --> (folds - 1) + (folds - 1) * (folds - 1)
  # in (folds - 1) outer folds in (folds - 2) inner folds and
  ttrain = table(unlist(train_sets))
  expect_true(length(unique(ttrain)) == 1 & unique(ttrain) == (folds - 1)^2)

  # Each observation is once in the outer test set: 1
  # In each inner loop it is once in the test set: (folds - 1)
  # Total appearances: folds
  ttest = table(unlist(test_sets))
  expect_true(length(unique(ttest)) == 1 & unique(ttest) == folds)

  all_disjoint = Reduce(`&&`, Map(function(x, y) length(intersect(x, y)) == 0, x = ttest, y = ttrain))
  expect_true(all_disjoint)

  # iter_info is correct

  for (i in seq_len(res$iters)) {
    info = res$unflatten(i)
    if (is.na(info$inner)) {
      expect_true((length(res$test_set(i)) + length(res$train_set(i))) == task$nrow)
    } else {
      expect_true((length(res$test_set(i)) + length(res$train_set(i))) < task$nrow)
    }
  }
})
