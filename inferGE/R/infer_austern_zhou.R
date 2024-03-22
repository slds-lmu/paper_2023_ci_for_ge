#' @export
infer_austern_zhou = function(x, y, alpha = 0.05, loss, ...) {
  assert_alpha(alpha)
  UseMethod("infer_austern_zhou")
}


#' @export
infer_austern_zhou.ResampleResult = function(x, y, alpha = 0.05, loss_fn = NULL, ...) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  assert_class(x$resampling, "ResamplingAusternZhou")
  assert(
    check_class(y$resampling, "ResamplingCV"),
    check_class(y$resampling, "ResamplingRepeatedCV")
  )
  if (test_class(y, "ResamplingRepeatedCV")) {
    assert_true(y$resampling$param_set$values$repeats == x$resampling$param_set$values$repeats)
  }
  assert_true(y$resampling$param_set$values$folds == x$resampling$param_set$values$folds)

  loss_table_var = calculate_loss(x$predictions("test"), loss_fn)
  loss_table_est = calculate_loss(y$predictions("test"), loss_fn)

  infer_austern_zhou(x = loss_table_var, y = loss_table_est, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_austern_zhou.loss_table = function(x, y, alpha = 0.05, loss, resampling) { # nolint
  assert_string(loss)
  assert_choice(loss, names(x))
  folds = resampling$param_set$values$folds
  repeats = resampling$param_set$values$repeats

  est_cv = y[, mean(get(loss))][[1]]
  n = resampling$task_nrow

  x = x[, c(loss, "row_id", "iter"), with = FALSE]
  names(x) = c("loss", "row_id", "iter")

  x[, cv_iter := (get("iter") - 1) %/% (folds * repeats) + 1]
  x$cv_iter = as.factor(x$cv_iter)

  # formula (6)
  mus = x[, list(loss = mean(loss)), by = "cv_iter"]

  mu_half = mus[1, loss]
  mu_half_repls = mus[-1, loss]

  # formula (27)
  s2_cv = n / 2 * sum((mu_half - mu_half_repls)^2)

  z = qnorm(1 - alpha / 2)

  halfwidth = z * sqrt(s2_cv / n)

  data.table(
    estimate = est_cv,
    lower = est_cv - halfwidth,
    upper = est_cv + halfwidth,
    info = list(list(
      mu_half = mu_half,
      mu_half_repls = mu_half_repls,
      s2_cv = s2_cv
    ))
  )
}
