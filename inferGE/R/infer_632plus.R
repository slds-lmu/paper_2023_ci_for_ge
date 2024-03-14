#' @export
infer_632plus = function(x, y, alpha = 0.05, ...) {
  assert_alpha(alpha)
  assert_class(y, "ResampleResult")
  assert_class(y$resampling, "ResamplingInsample")
  UseMethod("infer_632plus")
}

#' @include infer_ts_bootstrap.R
#' @export
infer_632plus.ResampleResult = function(x, y, alpha = 0.05, loss_fn = NULL) {
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  ci = infer_oob(x = x, alpha = alpha, loss_fn = loss_fn)

  lower = ci$lower
  upper = ci$upper
  err_oob = ci$estimate
  loss = names(loss_fn)

  insample_pred = y$predictions("test")
  loss_table_insample = calculate_loss(insample_pred, loss_fn)
  err_in = mean(loss_table_insample[[loss]])

  gamma = est_gamma(insample_pred[[1L]], loss_fn[[1L]])

  estimate = est_632plus(err_oob, err_in, gamma)

  lower = lower * estimate / err_oob
  upper = upper * estimate / err_oob

  data.table(
    estimate = estimate,
    lower = lower * estimate / err_oob,
    upper = upper * estimate / err_oob
  )
}
