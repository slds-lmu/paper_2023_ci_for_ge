#' @export
infer_bootstrap_ccv = function(x, alpha, ...) {
  UseMethod("infer_bootstrap_ccv")
}


#' @export
infer_bootstrap_ccv.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, ...) { # nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_bootstrap_ccv(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_bootstrap_ccv.loss_table = function(x, alpha = 0.05, loss = NULL, resampling, ...) { # nolint
  assert_numeric(alpha, len = 1L, lower = 0, upper = 1)
  assert_class(resampling, "ResamplingBootstrapCCV")
  assert_string(loss)
  assert_choice(loss, names(x))

  instance = resampling$instance
  M = instance$M

  # loss_weights indicates how often a sample is in the bootstrap sample.
  # Because we avoid making the same predictin multiple times during resample(), we here multiplu the loss
  # with the respective weight (see formula at bottom of page 5)
  loss_weights = as.vector(M)
  loss_weights = loss_weights[loss_weights != 0]

  x = x[, c(loss, "row_id", "iter"), with = FALSE]
  names(x) = c("loss", "row_id", "iter")

  x[, weight := loss_weights]
  x[, bootstrap_repeat := as.factor(instance$bootstrap_repeat)]

  mus = x[, list(mu = sum(loss * weight) / sum(weight)), by = "bootstrap_repeat"][["mu"]]

  # to obtain the CIs, we simply calculate the quantiles

  ci = quantile(x = mus, probs = c(alpha / 2, 1 - alpha / 2))

  data.table(
    estimate = mean(mus),
    lower = ci[[1]],
    upper = ci[[2]]
  )
}
