#' @export
infer_ts_boot = function(x, y, z, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_ts_boot")
}

#' @include infer_ls_boot.R
#' @export
infer_ts_boot.ResampleResult = function(x, y, z, alpha = 0.05, loss_fn = NULL) { #nolint
  assert_class(x$resampling, "ResamplingNestedBootstrap")
  assert_class(y$resampling, "ResamplingBootstrap")
  assert_class(z$resampling, "ResamplingInsample")
  assert_true(x$resampling$param_set$values$reps_inner == y$resampling$param_set$values$repeats)

  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  loss = names(loss_fn)

  reps_outer = x$resampling$param_set$values$reps_outer
  reps_inner = x$resampling$param_set$values$reps_inner

  test_predictions = x$predictions("test")

  estimates = map_dbl(seq_len(reps_outer), function(rep) {
    insample_pred = test_predictions[rep]

    loss_table_insample = calculate_loss(insample_pred, loss_fn)
    err_in = mean(loss_table_insample[[loss]])

    start = reps_outer + (rep - 1) * reps_inner + 1
    bootstrap_iters = seq(start, start + reps_inner - 1)
    loss_table_bootstrap = calculate_loss(test_predictions[bootstrap_iters], loss_fn)
    err_oob = mean(loss_table_bootstrap[, list(oob = mean(get(loss))), by = "row_id"]$oob)

    gamma = est_gamma(insample_pred[[1L]], loss_fn[[1L]])

    est_632plus(err_oob, err_in, gamma)
  })

  qs = unname(quantile(estimates, c(alpha / 2, 1 - alpha / 2)))

  # now use y and z to get the point estimate
  estimate = infer_632plus(x = y, y = z, loss_fn = loss_fn)$estimate

  data.table(
    estimate = estimate,
    lower = qs[1],
    upper = qs[2]
  )
}
