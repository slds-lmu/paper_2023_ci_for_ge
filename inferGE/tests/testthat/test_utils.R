test_that("utils works", {
  learner = lrn("classif.debug")
  task = tsk("iris")
  task$row_roles$use = c(1:3, 51:53, 101:103)
  resampling = rsmp("nested_cv", folds = 3)

  resampling$instantiate(task)

  rr = resample(task, learner, resampling)


  loss_fn = get_loss_fn("zero_one", rr)
  tbl = get_loss_table(rr, loss_fn)

  expect_data_table(tbl, ncol = 3, nrow = 27)
  expect_set_equal(names(tbl), c("iter", "row_id", "loss"))
})
