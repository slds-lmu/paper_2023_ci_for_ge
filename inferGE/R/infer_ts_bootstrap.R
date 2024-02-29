#' @export
infer_ts_boot = function(x, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_ts_boot")
}

#' @include infer_ls_boot.R
#' @export
infer_ts_boot.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  assert_class(x$resampling, "ResamplingNestedBootstrap")
  loss = names(loss_fn)

  reps_outer = x$resampling$param_set$values$reps_outer
  reps_inner = x$resampling$param_set$values$reps_inner

  estimates = map_dbl(seq_len(reps_outer), function(rep) {
    start = reps_outer + (rep - 1) * reps_inner + 1
    bootstrap_iters = seq(start, start + reps_inner - 1)
    insample_pred = x$predictions("test")[rep]
    loss_table_insample = calculate_loss(insample_pred, loss_fn)
    err_in = mean(loss_table_insample[[loss]])

    loss_table_bootstrap = calculate_loss(x$predictions("test")[bootstrap_iters], loss_fn)
    err_oob = mean(loss_table_bootstrap[, list(oob = mean(get(loss))), by = "iter"]$oob)

    gamma = est_gamma(insample_pred[[1L]]$truth, insample_pred[[1L]]$response, loss_fn[[1L]])

    est_632plus(err_oob, err_in, gamma)
  })

  qs = unname(quantile(estimates, c(alpha / 2, 1 - alpha / 2)))

  data.table(
    estimate = mean(estimates),
    lower = qs[1],
    upper = qs[2]
  )
}
