

#' @title Conservative Z-Test
#'
#' @description 
#' Conservative Z-Test.
#'
#' @param x ([`ResampleResult`] or  [`BenchmarkResult`])\cr
#'   The resample result or benchmark result 
#'
#' @export
infer_conservative_z = function(x, alpha = 0.05) {
  UseMethod("infer_conservative_z")
}

infer_conservative_z.BenchmarkResult = function(x, alpha = 0.05) {
  infer_method_bmr(x, alpha)
}


infer_conservative_z.ResampleResult = function(x, alpha = 0.05) {
  res = x$resampling
  assert_r6(res, "ResamplingSubsampling")
  assert_numeric(alpha, lower = 0, upper = 1)
}


