#' @export
infer_corrected_t = function(x, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_corrected_t")
}

#' @export
infer_corrected_t.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, ...) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn, task = x$task, resampling = x$resampling)

  infer_corrected_t(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_corrected_t.loss_table = function(x, alpha = 0.05, loss, resampling, ...) { # nolint
  # FIXME: if param_vals available
  expect_class(resampling, "ResamplingSubsampling")
  pv = resampling$param_set$values

  J = pv$repeats
  ratio = pv$ratio
  n = resampling$task_nrow

  # in the nadeau paper n1 is the train-set size and n2 the test set size

  n1 = round(ratio * n) # in the ResamplingSubsampling the same rounding is used
  n2 = n - n1

  loss_table = x

  # the different mu in the rows are the mu_j
  mus = loss_table[, list(estimate = mean(get(loss))), by = "iter"]
  # the global estimator
  estimate = mean(mus$estimate)
  # The naive SD estimate (does not take correlation between folds into account)
  estimate_sd = sd(mus$estimate)

  # The corrected SD estimate
  sd_corrected = estimate_sd * sqrt(1 / J + n2 / n1)
  z = qt(1 - alpha / 2, df = J - 1)

  halfwidth = sd_corrected * z

  lower = estimate - halfwidth
  upper = estimate + halfwidth

  data.table(
    estimate = estimate,
    lower = lower,
    upper = upper,
    info = list(list(
      variance_corrected = sd_corrected^2
    ))
  )
}
