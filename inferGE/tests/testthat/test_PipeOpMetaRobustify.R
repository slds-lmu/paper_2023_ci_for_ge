test_that("PipeOpMetaRobustify works", {
  po_meta = po("metarobustify")

  task = tsk("iris")
  task$row_roles$use = c(1, 1)
  task$row_roles$test = 2
  task$row_roles$holdout = 3

  taskout = po_meta$train(list(task))[[1L]]
  expect_identical(taskout$backend$rownames, c(1L, 2L))
  expect_identical(
    taskout$data(1),
    taskout$data(2)
  )

  graph = po("metarobustify") %>>% ppl("robustify") %>>% lrn("classif.rpart")

  rr = resample(tsk("iris"), graph, rsmp("bootstrap", repeats = 1, ratio = 1), stored)
  expect_class(rr, "ResampleResult")
})