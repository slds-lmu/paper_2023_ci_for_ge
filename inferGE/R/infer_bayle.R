#' @export
infer_bayle = function(x, alpha, loss, variance, ...) {
  UseMethod("infer_bayle")
}

#' @export
infer_bayle.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, variance = "all-pairs") { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_bayle(loss_table, alpha = alpha, loss = names(loss_fn), variance = variance, resampling = x$resampling)
}

#' @export
infer_bayle.loss_table = function(x, alpha = 0.05, loss, variance = "all-pairs", resampling) {
  assert_choice(variance, c("all-pairs", "within-fold"))
  assert_numeric(alpha, len = 1L, lower = 0, upper = 1)
  assert_class(resampling, "ResamplingCV")
  assert_string(loss)
  assert_choice(loss, names(x))

  n = resampling$task_nrow
  k = resampling$param_set$values$folds
  loss_table = x

  estimate = mean(loss_table[[loss]])

  var_est = if (variance == "within-fold") {
    # For ResamplingCV and LOO, iter are the folds
    loss_table[, list(var_fold = var(get(loss))), by = "iter"][, mean(get("var_fold"))]
  } else {
    mean((loss_table[[loss]] - estimate)^2)
  }

  se = sqrt(var_est / n)
  z = qnorm(1 - alpha / 2)
  halfwidth = se * z

  lower = estimate - halfwidth
  upper = estimate + halfwidth

  data.table(
    estimate = estimate,
    lower = lower,
    upper = upper,
    info = list(list(
      variance = var_est,
      method = variance
    ))
  )
}
