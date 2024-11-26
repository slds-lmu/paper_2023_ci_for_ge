test_that("PipeOpMetaRobustify works", {
  po_meta = PipeOpMetaRobustify$new()

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

  taskout2 = po_meta$predict(list(task))[[1L]]

  graph = po("metarobustify") %>>% ppl("robustify") %>>% lrn("classif.rpart")

  task = tsk("iris")
  task$row_roles$use = 1:140
  task$row_roles$holdout = 141:150

  learner = as_learner(graph)

  learner$predict_sets = c("test", "holdout")

  learner$train(task)

  rr = resample(task, learner, rsmp("bootstrap", repeats = 1, ratio = 1))
  expect_class(rr, "ResampleResult")
  expect_equal(length(rr$predictions("holdout")[[1]]$truth), 10)
})

test_that("ids remain intact when no duplicates", {
  po_meta = PipeOpMetaRobustify$new()
  task = tsk("iris")$filter(2:3)
  po_meta$train(list(task))
  taskout = po_meta$predict(list(task))[[1L]]
  expect_equal(taskout$row_ids, c(2, 3))
})

