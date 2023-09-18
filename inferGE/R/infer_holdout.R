#' @export
infer_holdout = function(x, alpha, loss, ...) { # nolint
  UseMethod("infer_holdout")
}

#' @export
infer_holdout.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, predict_set = "test", ...) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions(predict_set), loss_fn)

  infer_holdout(loss_table, alpha = alpha, loss = names(loss_fn))
}


#' @export
infer_holdout.loss_table = function(x, alpha = 0.5, loss = NULL, ...) { # nolint
  estimate = mean(x[[loss]])
  se = sd(x[[loss]]) / sqrt(nrow(x))

  z = qnorm(1 - alpha / 2)

  data.table(
    estimate = estimate,
    lower = estimate - se * z,
    upper = estimate + se * z
  )


}
