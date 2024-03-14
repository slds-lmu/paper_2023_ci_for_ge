test_that("can estimate overfitting rate", {
  task = tsk("spam")
  task2 = tsk("spam")
  task2$row_roles$use = rep(task2$row_roles$use, 2)
  pred = lrn("classif.rpart")$train(task)$predict(task2)
  # still reasonably fast, even with n \approx 10000
  expect_numeric(est_gamma(pred, mlr3measures::zero_one))
})

test_that("location shifted bootstrap", {
  learner = as_learner(PipeOpMetaRobustify$new() %>>%lrn("classif.rpart", predict_sets = c("train", "test")),)
  learner$predict_sets = c("train", "test")
  task = tsk("iris")
  boot = rsmp("bootstrap", ratio = 1, repeats = 50)
  insample = rsmp("insample")

  rr_boot = resample(task, learner, boot)
  rr_insample = resample(task, learner, insample)

  expect_ci_method(infer_ls_boot, rr_boot, y = rr_insample)
})

test_that("prob", {
  rr_boot = resample(tsk("iris"), lrn("classif.rpart", predict_type = "prob", predict_sets = c("train", "test")),
    rsmp("bootstrap"))
  rr_in = resample(tsk("iris"), lrn("classif.rpart", predict_type = "prob", predict_sets = c("train", "test")),
    rsmp("insample"))

  expect_data_table(infer_ls_boot(rr_boot, rr_in, loss_fn = list(logloss = logloss)))
})

