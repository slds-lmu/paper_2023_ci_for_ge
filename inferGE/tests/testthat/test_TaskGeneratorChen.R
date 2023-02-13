test_that("TaskGeneratorChen works", {
  M = 20
  gen = tgen("chen", M = M)
  task = gen$generate(100)
  expect_class(task, "TaskRegr")

  dat = task$data()
  expect_true(length(task$feature_names) == 6 * M)
})
