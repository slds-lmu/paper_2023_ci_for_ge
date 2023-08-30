#' @export
infer_subsampling = function(x, alpha, loss) { # nolint
  UseMethod("infer_subsampling")
}

#' @export
infer_subsampling.ResampleResult = function(x, alpha = 0.05, loss = NULL) { # nolint
  loss = get_loss_fn(loss, x)

  tbl = get_loss_table(x, loss)

  n_test = length(x$predictions()[[1]]$truth)

  # all repetitions have the same size so we can just aveage over everything
  estimate = mean(tbl$loss)

  sigmas = tbl[, sd(loss), by = iter]
  sigma_avg = sigmas[, mean(V1)]

  se_avg = sigma_avg / sqrt(n_test)

  c = qnorm(1 - alpha / 2)

  data.table(
    estimate = estimate,
    lower = estimate - c * se_avg,
    upper = estimate + c * se_avg
  )
}
