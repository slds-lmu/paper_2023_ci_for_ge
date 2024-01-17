#' @export
infer_holdout = function(x, alpha = 0.05, loss, ...) { # nolint
  assert_numeric(alpha, len = 1L, lower = 0, upper = 1)
  UseMethod("infer_holdout")
}

#' @export
infer_holdout.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_holdout(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}


#' @export
infer_holdout.loss_table = function(x, alpha = 0.05, loss, resampling) { # nolint
  assert_class(resampling, "ResamplingHoldout")
  assert_string(loss)
  assert_choice(loss, names(x))

  estimate = mean(x[[loss]])
  se = sd(x[[loss]]) / sqrt(nrow(x))

  z = qnorm(1 - alpha / 2)

  data.table(
    estimate = estimate,
    lower = estimate - se * z,
    upper = estimate + se * z
  )


}
