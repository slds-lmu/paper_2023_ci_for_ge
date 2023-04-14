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
infer_52cv = function(x, alpha = 0.05) {
  UseMethod("infer_52cv")
}
#' @export
infer_52cv.ResampleResult = function(x, alpha = 0.05, loss) { #nolint
  assert_class(x$resampling, "ResamplingRepeatedCV")
  assert_true(x$resampling)

}

#' @export
infer_52cv.BenchmarkResult = function(x, alpha = 0.05, loss) { #nolint
  infer_method_bmr(x, alpha, loss, infer_52cv)
}
