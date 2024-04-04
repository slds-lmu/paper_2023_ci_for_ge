#' @export
infer_bates = function(x, alpha = 0.05, ...) {
  assert_alpha(x = alpha)
  UseMethod("infer_bates")
}

#' @export
infer_bates.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  loss_table = calculate_loss(x$predictions("test"), loss_fn, task = task)
  infer_bates(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_bates.loss_table = function(x, alpha = 0.05, loss, resampling) { # nolint
  assert_class(resampling, "ResamplingNestedCV")
  assert_string(loss)
  assert_choice(loss, names(x))

  repeats = resampling$param_set$values$repeats
  folds = resampling$param_set$values$folds
  n_iters = resampling$iters
  n = resampling$task_nrow

  iter_info = map_dtr(seq_len(n_iters), function(i) {
    u = resampling$unflatten(i)
    n = x[list(i), get(".N"), on = "iter"][[1L]]
    list(
      rep   = rep(u$rep, times = n),
      outer = rep(u$outer, times = n),
      inner = rep(u$inner, times = n)
    )
  })
  x = cbind(x, iter_info)

  x_outer = x[is.na(get("inner"))]
  x_inner = x[!is.na(get("inner"))]

  b_list = x_outer[, list(x = var(get(loss)) / get(".N")), by = c("rep", "outer")][["x"]]

  tmp1 = x_outer[, list(avg_inner = mean(get(loss))), by = c("rep", "outer")]
  tmp2 = x_inner[, list(avg_outer = mean(get(loss))), by = c("rep", "outer")]
  tmp_join = merge(tmp1, tmp2, by = c("rep", "outer"))

  a_list = tmp_join[, list(a = (get("avg_inner") - get("avg_outer"))^2)][["a"]]

  err_ncv = mean(x_inner[[loss]])
  mse = mean(a_list - b_list)

  err_cv = x_outer[, mean(get(loss))]

  # left term going from (k -1) / k * n to k / n
  # right from going from (k - 2) / k * n to (k - 1) / l * n
  # different than in ntestedcv implementation but authors did not respond to my email
  bias = (1 + (folds - 2) / folds) * (err_ncv - err_cv)

  # We do the max(mse, 0) because the mse estimate can sometimes be negative.
  # The ensure_within ensures that it is within the range of assumig that all the estimates from the outer folds
  # are 100% dependent vs. assuming that they are 100% independent

  # we recommend re-scaling to obtain an estimate for a sample of size n by instead taking:
  mse = (folds - 1) / folds * mse

  sigma_in = sd(x_inner[[loss]])
  se_low = sigma_in / sqrt(n)
  se_up = se_low * sqrt(folds)

  se = max(se_low, min(sqrt(max(0, mse)), se_up))

  se_cv = sd(x_outer[[loss]]) / sqrt(n)

  s = qnorm(1 - alpha / 2) * se
  data.table(
    estimate = err_ncv - bias,
    lower = err_ncv - bias - s,
    upper = err_ncv - bias + s,
    info = list(list(
      bias = bias, # bias estimate ()
      mse_sqrt = suppressWarnings(sqrt(mse)), # estimate without correction, mse can be negative so suppress warnings
      se = se,
      err_ncv = err_ncv,
      err_cv = err_cv,
      se_cv = se_cv,
      adjusted = se != sqrt(max(mse, 0))
    ))
  )
}
