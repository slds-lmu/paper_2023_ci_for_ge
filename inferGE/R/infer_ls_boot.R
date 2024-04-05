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

  loss_table_test = calculate_loss(x$predictions("test"), loss_fn, task = x$task, resampling = x$resampling)
  loss_table_train = calculate_loss(x$predictions("train"), loss_fn, task = x$task, resampling = x$resampling)
  loss_table_insample = calculate_loss(y$predictions("test"), loss_fn, task = y$task, resampling = y$resampling)

  # loss_fn is a named list
  gamma = est_gamma(y$predictions("test")[[1L]], loss_fn[[1L]], task = y$task, train_set = y$resampling$train_set(1))

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

  bias = err_in - est

  qs = unname(quantile(errs_in, c(alpha / 2, 1 - alpha / 2)))
  data.table(
    estimate = est,
    lower = qs[1] - bias,
    upper = qs[2] - bias
  )
}

est_gamma = function(pred, loss, task, train_set) {
  n = length(pred$truth)
  prob_rep = if ("prob" %in% pred$predict_types) {
    prob_rep = matrix(rep(t(pred$prob), n), ncol = ncol(pred$prob), byrow = TRUE)
    colnames(prob_rep) = colnames(pred$prob)
    prob_rep
  }

  # train set is the same one for each permutation so it is sufficient to pass it once
  mean(loss(
    truth = rep(pred$truth, each = n),
    response = rep(pred$response, times = n),
    task = task,
    train_set = train_set,
    prob = prob_rep
  ))
}

est_632plus = function(err_oob, err_in, gamma) {
  R = (err_oob - err_in) / (gamma - err_in)
  w = 0.632 / (1 - 0.368 * R)
  (1 - w) * err_in + w * err_oob
}
