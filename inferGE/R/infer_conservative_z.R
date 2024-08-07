#' @title Conservative Z-Test
#'
#' @description
#' Conservative Z-Test.
#'
#' @param x ([`ResampleResult`] or  [`BenchmarkResult`])\cr
#'   The resample result or benchmark result
#'
#' @export
infer_conservative_z = function(x, alpha = 0.05, loss, ...) {
  assert_alpha(alpha)
  UseMethod("infer_conservative_z")
}

#' @export
infer_conservative_z.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) { # nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  loss_table = calculate_loss(x$predictions("test"), loss_fn, task = x$task, resampling = x$resampling)
  assert_class(x$resampling, "ResamplingConservativeZ")

  infer_conservative_z(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}


#' @export
infer_conservative_z.loss_table = function(x, alpha = 0.05, loss, resampling) {
  resampling_id = resampling$id
  resampling_params = resampling$param_set$values

  # this inference method implementation is a bit weird, because the corresponding Resampling method
  # does both the subsampling and the repeated paired subsampling
  # this is necessary however as they are nested

  J = resampling_params$J
  M = resampling_params$M

  tbl = x[, list(score = mean(get(loss))), by = "iter"]
  colnames(tbl) = c("iter", "measure")

  estimate = tbl[get("iter") <= J, mean(get("measure"))]

  # this table we use only to estimate the variance
  tbl_var = tbl[get("iter") > J, ]
  tbl_var$replication = rep(seq_len(M), each = J * 2)
  tbl_var$partition = rep(rep(1:2, each = J), times = M)

  # Now for each replication we average the measure in both partitions and then
  # calculate the squared error between the averaged partitions

  x = tbl_var[, mean(measure), by = .(replication, partition)]

  x = dcast(x, replication ~ partition, value.var = "V1")

  sigma2 = sum((x[["1"]] - x[["2"]])^2) / (2 * M)

  c = qnorm(1 - alpha / 2)

  data.table(
    estimate = estimate,
    lower = estimate - c * sqrt(sigma2),
    upper = estimate + c * sqrt(sigma2)
  )
}
