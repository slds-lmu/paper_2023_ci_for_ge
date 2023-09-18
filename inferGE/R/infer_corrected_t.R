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
infer_corrected_t = function(x, alpha = 0.05, loss, ...) {
  assert_numeric(alpha, lower = 0, upper = 1)
  UseMethod("infer_corrected_t")
}

#' @export
infer_corrected_t.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, predict_set = "test", ...) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions(predict_set), loss_fn)

  infer_corrected_t(loss_table, alpha = alpha, loss = names(loss_fn),
    resampling_info = list(id = x$resampling$id, params = x$resampling$param_set$values),
    task_info = list(nrow = x$task$nrow)
  )
}

#' @export
infer_corrected_t.loss_table = function(x, alpha = 0.05, loss, resampling_info, task_info, ...) { # nolint
  # FIXME: if param_vals available
  if (resampling_info$id != "subsampling") {
    stopf("Corrected t test works with subsampling.")
  }
  pv = resampling_info$params

  J = pv$repeats
  ratio = pv$ratio
  n = task_info$nrow

  # in the nadeau paper n1 is the train-set size and n2 the test set size

  n1 = round(ratio * n)
  n2 = n - n1

  loss_table = x

  browser()
  # the different mu in the rows are the mu_j
  mus = loss_table[, list(estimate = mean(get(loss))), by = "iter"]
  # the global estimator
  estimate = mean(mus$estimate)
  # The naive SD estimate (does not take correlation between folds into account)
  estimate_sd = sd(mus$estimate)

  # The corrected SD estimate
  sd_corrected = estimate_sd * sqrt(1 / J + n2 / n1)
  z = qnorm(1 - alpha / 2)

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
