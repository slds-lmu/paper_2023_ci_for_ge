
### test under asymptotic normality Bayles 2020
### resampling: custom (but should be cv or loo)
normality_bayles = function(data_raw) {
}

#' @export
infer_bayle = function(x, alpha = 0.05, loss, variance = "all-pairs") {
  UseMethod("infer_bayle")
}

#' @export
infer_bayle.BenchmarkResult = function(x, alpha = 0.05, loss, variance = "all-pairs") {
  infer_method_bmr(x, alpha, loss, infer_bayle)
}


#' @export
infer_bayle.ResampleResult = function(x, alpha = 0.05, loss, variance = "all-pairs") {
  assert_choice(variance, c("all-pairs", "within-fold"))
  assert_numeric(alpha, lower = 0, upper = 1)
  n = x$task$nrow
  k = x$resampling$param_set$values$folds
  if (variance == "within-fold") {
    assert_class(x$resampling, "ResamplingCV")
    assert_true(n >= 2 * k)
  } else {
    assert_true(inherits(x$resampling, "ResamplingCV") || inherits(x$resampling, "ResamplingLOO"))
  }
  loss_fn = get_loss_fn(loss, x)
  loss_table = get_loss_table(x, loss_fn)

  estimate = mean(loss_table$loss)

  var_est = if (variance == "within-fold") {
    # For ResamplingCV and LOO, iter are the folds
    tmp = loss_table[, j = list(loss = .N / (.N - 1) * (get("loss") - mean(get("loss")))^2), by = "iter"]
    1 / (n * k) * sum(tmp$loss)
  } else {
    mean((loss_table$loss - estimate)^2)
  }

  se = sqrt(var_est / n)
  z = abs(qnorm(alpha / 2))
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
