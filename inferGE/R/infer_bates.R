#' @title Inference Method for Nested Cross-Validation
#'
#' @description
#' For
#' @param x (`ResampleResult`)\cr
#'   The resample result using a `ResamplingNestedCV`.
#' @param alpha (`numeric(1)`)\cr
#'   The alpha level for the confidence interval.
#' @param loss (`character(1)`)\cr
#'   The observation loss. One of `mlr3measures::measures` that calculates oberservation-wise losses in an unaggergated
#'   manner. E.g. `"se"` for the squared error or `"zero_one"` for the 0-1 loss.
#'
#' @references
#' `r format_bib("bates2021")`
#'
#' @export
infer_bates = function(x, alpha, loss) {
  UseMethod("infer_bates")
}

#' @export
infer_bates.BenchmarkResult = function(x, alpha = 0.05, loss) {
  infer_method_bmr(x, alpha, loss, infer_bates)
}

#' @export
infer_bates.ResampleResult = function(x, alpha = 0.05, loss) {
  loss_fn = get_loss_fn(loss, x)

  res = x$resampling
  assert_r6(res, "ResamplingNestedCV")

  predictions = x$predictions()

  folds = sqrt(length(predictions))

  a_list = b_list = es_in = es_out = list()

  # TODO: Would be nicer to do it with data table ops like Lennart did for Bayle
  for (outer in seq_len(folds)) {
    # the outer resampling

    iter = res$flatten(outer, NULL)

    pred_outer = predictions[[iter]]
    pred_inner = map(seq_len(folds - 1), function(inner) predictions[[res$flatten(outer, inner)]])
    # FIXME: This will fail when the measure requires other information
    pointwise_outer_loss = loss_fn(truth = pred_outer$truth, response = pred_outer$response)
    pointwise_inner_loss = unlist(map(pred_inner, function(x) loss_fn(truth = x$truth, response = x$response)))

    a_list[[outer]] = (mean(pointwise_inner_loss) - mean(pointwise_outer_loss))^2
    b_list[[outer]] = var(pointwise_outer_loss) / length(pointwise_outer_loss)
    es_in[[outer]] = pointwise_inner_loss
    es_out[[outer]] = pointwise_outer_loss
  }

  # FIXME: This is sometimes negative
  mse = mean(unlist(a_list)) - mean(unlist(b_list))

  err_ncv = mean(unlist(es_in))
  err_cv = mean(unlist(es_out))

  bias = (1 + (folds - 2) / folds) * (err_ncv - err_cv)

  se_cv = sd(unlist(es_out)) / sqrt(folds)
  # We do the max(mse, 0) because the mse estimate can sometimes be negative.
  # The ensure_within ensures that it is within the range of assumig that all the estimates from the outer folds 
  # are 100% dependent vs. assuming that they are 100% independent
  se = ensure_within(sqrt(max(mse, 0)), se_cv, se_cv * sqrt(folds))
  halfwidth = abs(qnorm(alpha / 2)) * (folds - 1) / folds * se
  corrected_ncv = err_ncv - bias
  lower = corrected_ncv - halfwidth
  upper = corrected_ncv + halfwidth

  # FIXME: If we know the lower / upper bound of the loss_fn we should probably restrict the interval to take that
  # into account

  data.table(
    estimate = corrected_ncv,
    lower = lower,
    upper = upper,
    bias = bias,
    mse = mse, 
    err_ncv = err_ncv,
    err_cv = err_cv
  )
}
