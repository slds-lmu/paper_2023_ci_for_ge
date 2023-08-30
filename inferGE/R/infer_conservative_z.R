

#' @title Conservative Z-Test
#'
#' @description
#' Conservative Z-Test.
#'
#' @param x ([`ResampleResult`] or  [`BenchmarkResult`])\cr
#'   The resample result or benchmark result
#'
#' @export
infer_conservative_z = function(x, alpha = 0.05, loss) {
  UseMethod("infer_conservative_z")
}

#' @export
infer_conservative_z.BenchmarkResult = function(x, alpha = 0.05, loss) {
  infer_method_bmr(x, alpha)
}


#' @export
infer_conservative_z.ResampleResult = function(x, alpha = 0.05, loss) {
  res = x$resampling
  assert_r6(res, "ResamplingConservativeZ")
  assert_numeric(alpha, lower = 0, upper = 1)
  J = x$resampling$param_set$values$J
  M = x$resampling$param_set$values$M

  assert_choice(loss, c("se", "zero_one"))

  if (loss == "se") {
    measure = msr("regr.mse")
  } else {
    measure = msr("classif.acc")
  }


  # We use the first J resamplings to estimate the value and the rest for the variance

  tbl = as.data.table(x$score(measure))[, c("iteration", measure$id), with = FALSE]
  colnames(tbl) = c("iteration", "measure")

  estimate = tbl[iteration <= J, mean(measure)]

  # this table we use only to estimate the variance
  tbl_var = tbl[iteration > J, ]
  tbl_var$replication = rep(seq_len(M), each = J * 2)
  tbl_var$partition = rep(rep(1:2, each = J), times = M)

  # Now for each replication we average the measure in both partitions and then
  # calculate the squared error between the averaged partitions

  x = tbl_var[, mean(measure), by = .(replication, partition)]

  x = dcast(x, replication ~ partition, value.var = "V1")

  sigma2 = sum((x[["1"]] - x[["2"]])^2) / 2 * M

  c = qnorm(1 - alpha / 2)

  data.table(
    estimate = estimate,
    lower = estimate - c * sqrt(sigma2),
    upper = estimate + c * sqrt(sigma2)
  )

}


