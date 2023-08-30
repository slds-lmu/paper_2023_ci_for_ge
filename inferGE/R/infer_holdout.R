#' @export
infer_holdout = function(x, alpha, loss) { # nolint
  UseMethod("infer_holdout")
}

#' @export
infer_holdout.ResampleResult = function(x, alpha = 0.5, loss = NULL) { # nolint
  loss = loss %??% switch(x$task$task_type, regr = "se", classif = "zero_one", stop())
  assert_choice(loss, c("zero_one", "se"), null.ok = TRUE)

  loss_fn = get_loss_fn(loss, x)

  tbl = as.data.table(x$prediction())

  losses = loss_fn(truth = tbl$truth, response = tbl$response)

  estimate = mean(losses)

  se = sd(losses) / sqrt(length(losses))

  c = qnorm(1 - alpha / 2)

  data.table(
    estimate = estimate,
    lower = estimate - c * se,
    upper = estimate + c * se
  )
}
