#' @export
infer_bayle = function(x, alpha, loss, variance, ...) {
  UseMethod("infer_bayle")
}

#' @export
infer_bayle.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, predict_set = "test", variance = "all-pairs", ...) { #nolint
  #if (variance == "within-fold") {
  #  assert_class(x$resampling, "ResamplingCV")
  #  assert_true(n >= 2 * k)
  #} else {
  #  assert_true(inherits(x$resampling, "ResamplingCV") || inherits(x$resampling, "ResamplingLOO"))
  #}

  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions(predict_set), loss_fn)

  infer_bayle(loss_table, alpha = alpha, loss = names(loss_fn), variance = variance)
}

#' @export
infer_bayle.loss_table = function(x, alpha = 0.05, loss, variance = "all-pairs", ...) {
  assert_choice(variance, c("all-pairs", "within-fold"))
  assert_numeric(alpha, lower = 0, upper = 1)

  n = length(unique(x$row_id))
  k = max(x$iter)
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
