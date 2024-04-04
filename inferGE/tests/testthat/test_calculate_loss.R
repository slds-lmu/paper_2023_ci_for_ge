test_that("calculate_loss works", {
  rr = resample(
    task = tsk("iris"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("cv")
  )

  loss_table = calculate_loss(rr$predictions(), list(zero_one = mlr3measures::zero_one), task = tsk("iris"))

  expect_data_table(loss_table, ncols = 3L, nrows = 150)
  expect_set_equal(colnames(loss_table), c("zero_one", "iter", "row_id"))
})
