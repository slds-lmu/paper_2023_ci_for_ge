#' @title Corrected T-Test
#' @description
#' Applies the corrected T-Test.
#'
#' @param x ([`ResampleResult`] or  [`BenchmarkResult`])\cr
#'   The resampling must be a [`ResamplingSubsampling`] with more than one repetition (parameter `repeats`).
#' @param alpha (`numeric(1)`)\cr
#'   The alpha level for the confidence interval.
#'
#' @param loss
#'
#' @references
#' `r format_bib("nadeau1999inference")`
#' @export
infer_corrected_t = function(x, alpha = 0.05, loss) {
  UseMethod("infer_corrected_t")
}

#' @export
infer_corrected_t.BenchmarkResult = function(x, alpha = 0.05, loss) { # nolint
  infer_method_bmr(x, alpha, loss)
}

#' @export
infer_corrected_t.ResampleResult = function(x, alpha = 0.05, loss) { # nolint
  res = x$resampling
  assert_r6(res, "ResamplingSubsampling")
  assert_numeric(alpha, lower = 0, upper = 1)
  # FIXME: if param_vals available
  pv = res$param_set$values
  J = pv$repeats
  ratio = pv$ratio
  n = res$task_nrow

  # in the nadeau paper n1 is the train-set size and n2 the test set size

  n1 = round(ratio * res$task_nrow)
  n2 = n - n1

  n1 = (1 - ratio) * res$task_nrow

  loss_fn = get_loss_fn(loss, x)
  loss_table = get_loss_table(x, loss_fn)

  # the different mu in the rows are the mu_j
  mus = loss_table[, list(estimate = mean(get("loss"))), by = "iter"]
  # the global estimator
  estimate = mean(mus$estimate)
  # The naive SD estimate (does not take correlation between folds into account)
  estimate_sd = sd(mus$estimate)

  # The corrected SD estimate
  sd_corrected = estimate_sd * sqrt(1 / J + n2 / n1)
  z = abs(qnorm(min(alpha, 1 - alpha) / 2))

  halfwidth = sd_corrected * z

  lower = estimate - halfwidth
  upper = estimate + halfwidth

  data.table(
    estimate = estimate,
    lower = lower,
    upper = upper,
    variance = sd_corrected^2,
    method = "corrected_t"
  )

}
