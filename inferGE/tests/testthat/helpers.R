lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

expect_ci_method = function(inference, rr, ..., .symmetric = TRUE) {
  r05 = inference(rr, alpha = 0.05, ...)
  r05_def = inference(rr, ...)

  # default is 0.05
  expect_equal(r05$lower, r05_def$lower)
  expect_equal(r05$upper, r05_def$upper)

  expect_data_table(r05, nrows = 1L)
  expect_subset(colnames(r05), c("estimate", "lower", "upper", "info"))
  expect_subset(c("estimate", "lower", "upper"), colnames(r05))
  expect_true(r05$lower < r05$estimate)
  expect_true(r05$upper > r05$estimate)


  # alpha level is taken into account
  r10 = inference(rr, alpha = 0.1, ...)
  expect_true((r10$upper - r10$lower) < (r05$upper - r05$lower))

  other_loss = switch(rr$task_type,
    classif = list(one_zero = function(truth, response, ...) as.integer(truth == response)),
    regr = list(ae = mlr3measures::ae),
    stop("wrong task type")
  )

  if (.symmetric) {
    expect_equal(
      r05$upper - r05$estimate,
      r05$estimate - r05$lower
    )
  }


  dots = list(...)
  dots$loss_fn = other_loss

  r05_other_loss = invoke(inference, x = rr, alpha = 0.05, .args = dots)
  expect_true(r05$estimate != r05_other_loss$estimate)

}
