#' @export
#' @param y
infer_bootstrap_ccv = function(x, y = NULL, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_bootstrap_ccv")
}


#' @export
infer_bootstrap_ccv.ResampleResult = function(x, y = NULL, alpha = 0.05, loss_fn = NULL, ...) { # nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  if (!is.null(y)) {
    assert_class(y, "ResampleResult")
    assert(
      check_class(y$resampling, "ResamplingCV"),
      check_class(y$resampling, "ResamplingRepeatedCV"),
      check_class(y$resampling, "ResamplingLOO"),
    )
  }


  loss_table = calculate_loss(x$predictions("test"), loss_fn)
  est = if (!is.null(y)) {
    mean(loss_table[[names(loss_fn)[1L]]])
  }

  infer_bootstrap_ccv(loss_table, est = est, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_bootstrap_ccv.loss_table = function(x, est = NULL, alpha = 0.05, loss = NULL, resampling, ...) { # nolint
  # y is being used to estimate the bias
  assert_string(loss)
  assert_choice(loss, names(x))

  instance = resampling$instance
  M = instance$M

  # loss_weights indicates how often a sample is in the bootstrap sample.
  # Because we avoid making the same predictin multiple times during resample(), we here multiply the loss
  # with the respective weight (see formula at bottom of page 5)
  loss_weights = as.vector(M)
  loss_weights = loss_weights[loss_weights != 0]

  x = x[, c(loss, "row_id", "iter"), with = FALSE]
  names(x) = c("loss", "row_id", "iter")

  x[, weight := loss_weights]
  x[, bootstrap_repeat := as.factor(instance$bootstrap_repeat)]

  mus = x[, list(mu = sum(loss * weight) / sum(weight)), by = "bootstrap_repeat"][["mu"]]

  est_bccv = mean(mus)

  bias = if (!is.null(est)) est_bccv - est else 0

  qs = unname(quantile(x = mus, probs = c(alpha / 2, 1 - alpha / 2)))

  data.table(
    estimate = est %??% est_bccv,
    lower = qs[[1]] - bias,
    upper = qs[[2]] - bias
  )
}
