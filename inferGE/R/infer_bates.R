#' @title Inference Method for Nested Cross-Validation
#'
#' @description
#' Inference method for Nested Cross-Validation.
#'
#' @param x ([`ResampleResult`] or  [``])\cr
#'   The resample result or benchmark result using a [`ResamplingNestedCV`] as the resampling technique.
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
infer_bates = function(x, alpha = 0.05, loss, ...) {
  UseMethod("infer_bates")
}


#' @export
infer_bates.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_bates(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_bates.loss_table = function(x, alpha = 0.05, loss, resampling) { # nolint
  assert_numeric(alpha, len = 1L, lower = 0, upper = 1)
  assert_class(resampling, "ResamplingNestedCV")
  assert_string(loss)
  assert_choice(loss, names(x))

  folds = resampling$param_set$values$folds
  n_iters = resampling$iters

  sizes = x[, get(".N"), by = "iter"][[2]]

  # get information about inner / outer
  tmp = lapply(seq_len(n_iters), function(iter) {
    u = resampling$unflatten(iter)
    list(
      outer = rep(u$outer, times = sizes[iter]),
      inner = rep(u$inner, times = sizes[iter])
    )
  })

  new_cols = rbindlist(tmp)
  x = cbind(x, new_cols)

  x_outer = x[is.na(get("inner"))]
  x_inner = x[!is.na(get("inner"))]

  b_list = x_outer[, list(x = var(get(loss)) / get(".N")), by = outer][["x"]]

  tmp1 = x_outer[, list(avg_inner = mean(get(loss))), by = "outer"]
  tmp2 = x_inner[, list(avg_outer = mean(get(loss))), by = "outer"]
  tmp_join = merge(tmp1, tmp2, on = "outer")

  a_list = tmp_join[, list(a = (avg_inner - avg_outer)^2)][["a"]]

  err_ncv = mean(x_inner[[loss]])
  mse = mean(a_list) - mean(b_list)

  err_cv = x_outer[, list(x = mean(get(loss)))][["x"]]

  # left term going from goning from (k -1) / k * n to k / n
  # right from going from (k - 2) / k * n to (k - 1) / l * n
  bias = (1 + (folds - 2) / folds) * (err_ncv - err_cv)

  # We do the max(mse, 0) because the mse estimate can sometimes be negative.
  # The ensure_within ensures that it is within the range of assumig that all the estimates from the outer folds
  # are 100% dependent vs. assuming that they are 100% independent

  # we recommend re-scaling to obtain an estimate for a sample of size n by instead taking:
  mse = (folds - 1) / folds * mse

  N = nrow(x_outer)
  se_cv = sd(x_outer[[loss]]) / sqrt(N)

  # As a minor detail, in practice we also restrict
  mse_sqrt_corrected = ensure_within(
    x = sqrt(max(mse, 0)),
    lower = se_cv,
    upper = se_cv * sqrt(folds)
  )


  # whether we didn't really use nested CV
  adjusted = mse_sqrt_corrected != sqrt(max(mse, 0))
  s = qnorm(1 - alpha / 2) * mse_sqrt_corrected

  data.table(
    estimate = err_ncv - bias,
    lower = err_ncv - bias - s,
    upper = err_ncv - bias + s,
    info = list(list(
      bias = bias, # bias estimate ()
      mse_sqrt = suppressWarnings(sqrt(mse)), # estimate without correction, mse can be negative so suppress warnings
      mse_sqrt_corrected = mse_sqrt_corrected,
      err_ncv = err_ncv,
      err_cv = err_cv,
      se_cv = se_cv,
      adjusted = adjusted
    ))
  )
}
