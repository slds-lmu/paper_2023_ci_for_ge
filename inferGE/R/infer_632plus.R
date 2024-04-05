#' @export
infer_632plus = function(x, y, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_632plus")
}

#' @include infer_ts_bootstrap.R
#' @export
infer_632plus.ResampleResult = function(x, y, alpha = 0.05, loss_fn = NULL) {
  assert_class(x, "ResampleResult")
  assert_class(x$resampling, "ResamplingBootstrap")
  assert_class(y, "ResampleResult")
  assert_class(y$resampling, "ResamplingInsample")

  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  ci = infer_oob(x = x, alpha = alpha, loss_fn = loss_fn)

  SE_del = ci$info[[1L]]$SE_del
  err_oob = ci$estimate
  loss = names(loss_fn)

  insample_pred = y$predictions("test")
  loss_table_insample = calculate_loss(insample_pred, loss_fn, task = y$task, resampling = y$resampling)
  err_in = mean(loss_table_insample[[loss]])

  gamma = est_gamma(insample_pred[[1L]], loss_fn[[1L]], task = y$task, train_set = y$resampling$train_set(1))

  estimate = est_632plus(err_oob, err_in, gamma)

  se_new = SE_del * estimate / err_oob

  z = qnorm(1 - alpha / 2)
  data.table(
    estimate = estimate,
    lower = estimate - z * se_new,
    upper = estimate + z * se_new
  )
}
