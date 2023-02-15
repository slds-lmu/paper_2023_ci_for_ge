test_that("ResamplingBootstrapCCV works", {
  res = rsmp("bootstrap_ccv", ratio = 1, repeats = 2)
  task = tsk("iris")
  task$row_roles$use = c(1:5, 51:55, 101:105)
  res #check that printer works (because iters are NA)
  expect_true(is.na(res$iters))
  res$instantiate(task)
  inst = res$instance

  expect_list(inst)
  expect_set_equal(names(inst), c("row_ids", "M", "holdout", "bootstrap_repeat"))

  expect_true(res$iters == sum(inst$M != 0))
  expect_true(res$iters == length(inst$holdout))
  expect_true(res$iters == length(inst$bootstrap_repeat))

  res$param_set$set_values(ratio = 0)
  expect_error(res$instantiate(task))

  train_sets = map(seq_len(res$iters), function(i) get_private(res)$.get_train(i))
  test_sets = map(seq_len(res$iters), function(i) get_private(res)$.get_test(i))

  totals = map(seq_len(res$iters), function(i) unique(c(train_sets[[i]], test_sets[[i]])))

  # test that train and test sets are disjoint and their unions are equal to the bootstrap samples
  walk(seq_len(res$iters), function(i) {
    expect_true(length(intersect(train_sets[[i]], test_sets[[i]])) == 0)
    bootstrap_repeat = res$instance$bootstrap_repeat[i]
    expected = res$instance$row_ids[which(res$instance$M[, bootstrap_repeat] != 0)]
    expect_set_equal(totals[[i]], expected)
  })

  rr = resample(task, lrn("classif.featureless"), res = rsmp("bootstrap_ccv", ratio = 1, repeats = 2))


  expect_class(rr, "ResampleResult")
})

test_that("other ratios work as well", {
  task = tsk("penguins")
  l = lrn("classif.featureless")
  res = rsmp("bootstrap_ccv", ratio = 0.1, repeats = 1)
  rr = resample(task, l, res)
  expect_class(rr, "ResampleResult")
})

test_that("Works with grouping", {
  res = rsmp("bootstrap_ccv", ratio = 1, repeats = 2)
  task = tsk("penguins")
  task$set_col_roles("island", "group")

  rr = resample(task, lrn("classif.featureless"), res)
  expect_class(rr, "ResampleResult")
})


test_that("Works with stratification", {
  res = rsmp("bootstrap_ccv", ratio = 0.1, repeats = 1)
  task = tsk("penguins")
  task$set_col_roles("island", "stratum")

  rr = resample(task, lrn("classif.featureless"), res)
  expect_class(rr, "ResampleResult")
})

test_that("Resampling iterations with either no train or no test observations are removed", {
  task = tsk("iris")
  task$row_roles$use = 1
  res = rsmp("bootstrap_ccv", ratio = 1, repeats = 2)
  res$instantiate(task)
  expect_true(res$iters == 0)
})

test_that("Degenerate samples can be combined to proper samples", {
  task = tsk("penguins")
  task$row_roles$use = 1:3
  task$col_roles$stratum = "sex"

  # In an earlier implementation this would have caused a warning because in one of the strata 
  # we either have no train or test oberervation.

  res = rsmp("bootstrap_ccv", ratio = 1, repeats = 100)
  res$instantiate(task)
  expect_true(res$iters > 0)
})
