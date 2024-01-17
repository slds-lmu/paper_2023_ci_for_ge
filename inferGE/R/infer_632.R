#' @export
#' @include utils.R
infer_632 = function(x, y, alpha = 0.05, loss, ...) { # nolint
  assert_alpha(alpha)
  UseMethod("infer_632")
}

#' @export
infer_632.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_632(loss_table, alpha = alpha, loss = names(loss_fn), resampling = list(x = x$resampling, y = y$resampling))
}

#' @export
infer_632.loss_table = function(x, alpha = 0.05, loss, resampling) { # nolint
  assert_list(resampling, "ResamplingBootstrap")
  assert_string(loss)
  assert_choice(loss, names(x))
  assert_list()


  estimate = mean(x[[loss]])
  se = sd(x[[loss]]) / sqrt(nrow(x))

  z = qnorm(1 - alpha / 2)

  data.table(
    estimate = estimate,
    lower = estimate - se * z,
    upper = estimate + se * z
  )
}
