#' @export
infer_austern_zhou = function(x, alpha, loss, ...) {
  UseMethod("infer_austern_zhou")
}


#' @export
infer_austern_zhou.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, ...) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions("test"), loss_fn)

  infer_austern_zhou(loss_table, alpha = alpha, loss = names(loss_fn), resampling = x$resampling)
}

#' @export
infer_austern_zhou.loss_table = function(x, alpha = 0.05, loss, resampling) { # nolint
  assert_numeric(alpha, len = 1L, lower = 0, upper = 1)
  assert_class(resampling, "ResamplingAusternZhou")
  assert_string(loss)
  assert_choice(loss, names(x))
  folds = resampling$param_set$values$folds

  n = resampling$task_nrow

  # renaming because dt release does not come :(
  x = x[, c(loss, "row_id", "iter"), with = FALSE]
  names(x) = c("loss", "row_id", "iter")

  x[, cv_iter := (get("iter") - 1) %/% get("folds") + 1]
  x$cv_iter = as.factor(x$cv_iter)

  # formula (6)
  mus = x[, list(loss = mean(loss)), by = "cv_iter"]

  mu_full = mus[1, loss]
  mu_half = mus[2, loss]
  mu_half_repls = mus[-(1:2), loss]

  # formula (27)
  s2_cv = mean((mu_half - mu_half_repls)^2)

  z = qnorm(1 - alpha / 2)

  halfwidth = z * sqrt(s2_cv / n)

  data.table(
    estimate = mu_full,
    lower = mu_full - halfwidth,
    upper = mu_full + halfwidth,
    info = list(list(
      mu_half = mu_half,
      mu_half_repls = mu_half_repls,
      s2_cv = s2_cv
    ))
  )
}
