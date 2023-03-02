test_that("infer_bayle works", {
  learner = lrn("classif.rpart")
  task = tsk("penguins")
  resampling = rsmp("cv", folds = 10)

  rr = resample(task, learner, resampling)

  tbl = infer_bayle(rr, loss = "zero_one", variance = "within-fold")

  expect_data_table(tbl, nrow = 1, ncol = 5)
  expect_set_equal(
    colnames(tbl), 
    c("estimate", "lower", "upper", "variance", "method")
  )
})
