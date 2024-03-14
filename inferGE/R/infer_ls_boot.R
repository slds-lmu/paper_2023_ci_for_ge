#' @export
infer_ls_boot = function(x, y, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_ls_boot")
}

#' @export
infer_ls_boot.ResampleResult = function(x, y, alpha = 0.05, loss_fn = NULL) { #nolint
  assert_class(x, "ResampleResult")
  assert_class(x$resampling, "ResamplingBootstrap")
  assert_class(y, "ResampleResult")
  assert_class(y$resampling, "ResamplingInsample")
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)
  # train predictions are present
  assert_false(isTRUE(all.equal(length(x$predictions("train")[[1L]]), list())))

  loss_table_test = calculate_loss(x$predictions("test"), loss_fn)
  loss_table_train = calculate_loss(x$predictions("train"), loss_fn)
  loss_table_insample = calculate_loss(y$predictions("test"), loss_fn)

  # loss_fn is a named list
  gamma = if ("prob" %in% y$predictions("test")[[1L]]$predict_types) {
    est_gamma(truth = y$prediction("test")$truth, prob = y$prediction("test")$prob, response = y$prediction("test")$response,
      loss  = loss_fn[[1L]])
  } else {
    est_gamma(truth = y$prediction("test")$truth, response = y$prediction("test")$response, loss  = loss_fn[[1L]])
  }

  infer_ls_boot(x = loss_table_test, y = loss_table_train, z = loss_table_insample, gamma = gamma,
    alpha = alpha, loss = names(loss_fn))
}

#' @export
infer_ls_boot.loss_table = function(x, y, z, gamma, alpha = 0.05, loss) {
  # Use definition from Efron, but remove terms that are 0
  err_oob = mean(x[, list(oob = mean(get(loss))), by = "row_id"]$oob)
  errs_in = y[, list(err_in = mean(get(loss))), by = "iter"]$err_in
  err_in = mean(z[[loss]])
  est = est_632plus(err_oob, err_in, gamma)

  bias = err_in - err_oob

  qs = unname(quantile(errs_in, c(alpha / 2, 1 - alpha / 2)))
  data.table(
    estimate = est,
    lower = qs[1] - bias,
    upper = qs[2] - bias
  )
}

est_gamma = function(truth, response, prob = NULL, loss) {
  mean(map_dbl(seq_along(truth), function(i) {
    if (!is.null(prob)) {
      mean(loss(truth = rep(truth[i], length(response)), response = response, prob = prob))
    } else {
      mean(loss(truth = rep(truth[i], length(response)), response = response))
    }
  }))
}

est_632plus = function(err_oob, err_in, gamma) {
  R = (err_oob - err_in) / (gamma - err_in)
  w = 0.632 / (1 - 0.368 * R)
  (1 - w) * err_in + w * err_oob
}
