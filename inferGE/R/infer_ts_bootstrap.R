#' @export
infer_ts_boot = function(x, y, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_ts_boot")
}

#' @export
infer_ts_boot.ResampleResult = function(x, y, alpha = 0.05, loss_fn = NULL) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  expect_true(length(x$prediction("train"))[[1L]] > 0L)
  loss_table_test = calculate_loss(x$predictions("test"), loss_fn)
  loss_table_train = calculate_loss(x$predictions("train"), loss_fn)

  infer_ts_boot(loss_table, alpha = alpha, loss = names(loss_fn), variance = variance, resampling = x$resampling)
}

#' @export
infer_ts_boot.loss_table = function(x, y, alpha = 0.05, loss, variance = "all-pairs", resampling) {


  # apply 632 estimator to each repetition

  data.table(
    estimate = ,
    lower =,
    upper =
  )
}
