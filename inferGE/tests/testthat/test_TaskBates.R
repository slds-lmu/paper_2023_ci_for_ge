test_that("TaskGeneratorBatesClassif works", {
  gen = tgen("classif.bates", ones = 5, zeros = 15, c = 1)

  n = 10001
  task = gen$generate(n)
  expect_class(task, "TaskClassif")
  expect_true(length(task$class_names) == 2L)
  expect_true(length(task$feature_names) == 20)
  expect_true(task$nrow == n)
  expect_true(task$id == "classif.bates")

  dat = task$data()

  ynum = ifelse(dat$y == "0", 0, 1)
  # mean should be 0
  expect_true(abs(mean(ynum) - 0.5) < 0.05)
})

test_that("TaskGeneratorBatesRegr works", {
  gen = tgen("regr.bates", ones = 4, zeros = 17, c = 1)

  n = 10002
  task = gen$generate(n)
  expect_class(task, "TaskRegr")
  expect_true(length(task$feature_names) == 21)
  expect_true(task$nrow == n)
  expect_true(task$id == "regr.bates")

  dat = task$data()


  # mean should be 0
  expect_true(mean(dat$y) < 0.05)
})
