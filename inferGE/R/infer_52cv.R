#' @title Paired T-Test with 5x2 Cross-Validation
#'
#' @description
#' Approximate t-test using 5x2 Cross-Validation.
#'
#' @param x ([`ResampleResult`] or  [`BenchmarkResult`])\cr
#'   The resample result or benchmark result using a [`ResamplingNestedCV`] as the resampling technique.
#' @template param_alpha
#' @param loss (`character(1)`)\cr
#'   The observation loss. One of `mlr3measures::measures` that calculates oberservation-wise losses in an unaggergated
#'   manner. E.g. `"se"` for the squared error or `"zero_one"` for the 0-1 loss.
#'
#' @references
#' `r format_bib("dietterich1998approximate")`
infer_52cv = function(x, alpha = 0.05, loss) {
  UseMethod("infer_52cv")
}
#' @export
infer_52cv.ResampleResult = function(x, alpha = 0.05, loss) { #nolint
  assert_class(x$resampling, "ResamplingRepeatedCV")
  assert_true(x$resampling$param_set$values$folds == 2L)
  assert_true(x$resampling$param_set$values$repeats == 5L)

  # TODO: fix later
  assert_choice(loss, c("se", "zero_one"))

  if (loss == "se") {
    measure = msr("regr.mse")
  } else {
    measure = msr("classif.acc")
  }

  # iters 1, 2 are from the first repeat, 3, 4 from the second etc.
  tbl = as.data.table(x$score(measure))[, c("iteration", measure$id), with = FALSE]
  tbl$iteration = NULL
  names(tbl) = "measure"
  tbl$replication = rep(1:5, each = 2)
  tbl$fold = rep(1:2, times = 5)

  # only take the estimate on the first fold from the first replication
  estimate = tbl[1, "measure"][[1L]]

  # first we calculate the mean over both folds for each replication
  tbl[, measure_avg := mean(measure), by = replication]

  # formula: sqrt(1/5 * sum{i = 1, ...5 } (measure1_i - measure_avg_i)^2 + (measure2_i - measure_avg_i)^2)
  s = tbl[, sqrt(1/5 * sum((measure - measure_avg)^2))]

  z = qt(1 - alpha / 2, df = 5)

  data.table(
    estimate = estimate,
    lower = estimate - s * z,
    upper = estimate + s * z
  )
}

#' @export
infer_52cv.BenchmarkResult = function(x, alpha = 0.05, loss) { #nolint
  infer_method_bmr(x, alpha, loss, infer_52cv)
}
