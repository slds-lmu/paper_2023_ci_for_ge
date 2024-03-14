#' @export
infer_52cv = function(x, alpha = 0.05, ...) {
  assert_alpha(alpha)
  UseMethod("infer_52cv")
}

#' @export
infer_52cv.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_52cv(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_52cv.loss_table = function(x, alpha = 0.05, loss, resampling) {
  browser()
  assert_class(resampling, "ResamplingRepeatedCV")
  assert_true(resampling$param_set$values$repeats == 5L && resampling$param_set$values$folds == 2L)
  assert_string(loss)
  assert_choice(loss, names(x))

  fold_name = make.unique(c(colnames(x), "fold"))[ncol(x) + 1L]
  replication_name = make.unique(c(colnames(x), "replication"))[ncol(x) + 1L]

  # first we calculate the mean over both folds for each replication
  ps = x[, list(avg = mean(get(loss))), by = "iter"]
  set(ps, j = fold_name, value = rep(1:2, times = 5))
  set(ps, j = replication_name, value = rep(1:5, each = 2))

  p11 = ps[get(fold_name) == 1L & get(replication_name) == 1L, ]$avg

  ps[, `:=`(avg = get("avg"), repl_avg = mean(get("avg"))), by = c(replication_name)]

  s = ps[, sqrt(1/5 * sum((get("avg") - get("repl_avg"))^2))]

  z = qt(1 - alpha / 2, df = 5)

  data.table(
    estimate = unname(p11),
    lower = p11 - s * z,
    upper = p11 + s * z
  )
}
