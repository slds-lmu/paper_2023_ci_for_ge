test_that("infer_bayle works", {
  learner = lrn("classif.rpart")
  task = tsk("penguins")
  resampling = rsmp("cv", folds = 10)

  rr = resample(task, learner, resampling)

  tbl = infer_bayle(rr, loss = "zero_one")

  tbl = get_loss_table(rr, "zero_one")
  
})
