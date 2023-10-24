#' @export
infer_naive_cv = function(x, alpha = 0.05, ...) {
  UseMethod("infer_naive_cv")
}

#' @export
infer_naive_cv.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) {
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_naive_cv(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_naive_cv.loss_table = function(x, alpha = 0.05, loss = NULL, resampling) {
  assert_numeric(alpha, len = 1L, lower = 0, upper = 1)
  assert_class(resampling, "ResamplingCV")
  assert_true(resampling$iters > 2L)
  assert_string(loss)
  assert_choice(loss, names(x))

  k = resampling$param_set$values$folds

  x = x[, c(loss, "row_id", "iter"), with = FALSE]
  colnames(x) = c("loss", "row_id", "iter")


  mu = x[, mean(loss)]
  mus = x[, mean(loss), by = "iter"]

  se = sqrt(1 / (k - 1) * k * sum((mus - mu)^2))

  z = qnorm(1 - alpha / 2)

  data.table(
    estimate = mu,
    lower = mu - z * se,
    upper = mu + z * se
  )
}
