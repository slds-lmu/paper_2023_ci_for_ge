confidence_interval = function(resample_result, method, alpha, ...) {
  switch(method,
    naive = confidence_interval_naive(resample_result, alpha = alpha, ...),
    nested = confidence_interval_nested(resample_result, alpha = alpha, ...)
  )
}

preprocess_naive = function(resample_result, alpha) {



  list(err = err, mse = mse)
}

#' @description
#' For
#' @param resample_result (`ResampleResult`)\cr
#'  The resample result using a `ResamplingNestedCV`.
#'
ci_nested_cv = function(resample_result, alpha = 0.05, obs_loss = mlr3measures::se) {
  res = resample_result$resampling
  assert_r6(res, "ResamplingNestedCV")
  folds = res$param_set$values$folds
  predictions = resample_result$predictions()

  a_list = b_list = es_in = es_out =  list()

  for (outer in seq_len(folds)) {
    # the outer resampling

    iter = res$outer_inner_to_iter(outer, NULL)

    pred_out = predictions[[iter]]
    pred_inner = map(
      seq_len(folds - 1),
      function(inner) {
        iter = res$outer_inner_to_iter(outer, inner)
        predictions[[iter]]
      }
    )
    e_out = obs_loss(pred_out$truth, pred_out$response)
    e_inner = unlist(map(pred_inner, function(x) obs_loss(x$truth, x$response)))

    a_list[[outer]] = (mean(e_inner) - mean(e_out))^2
    b_list[[outer]] = var(e_out) / length(e_out)

    es_out[[outer]] = e_out

    es_in[[outer]] = e_inner
  }

  mse = mean(unlist(a_list)) - mean(unlist(b_list))
  err_ncv = mean(unlist(es_in))

  err_cv = mean(unlist(es_out))

  # Average over observations or over fols?
  bias = (1 + (folds - 2) / folds) * (err_ncv - err_cv)

  halfwidth = qnorm(alpha / 2) * (folds - 1) / folds * sqrt(mse)
  corrected_ncv = err_ncv - bias

  lower = corrected_ncv - halfwidth
  upper = corrected_ncv + halfwidth

  list(err_ncv = err_ncv, lower_ncv = lower, upper_ncv = upper, err_cv = err_cv, mse = mse, bias = bias)
}

if (FALSE) {
  library(mlr3verse)
  set.seed(1)
  task = tsk("boston_housing")
  task$col_roles$feature = setdiff(task$col_roles$feature, c("chas", "town"))
  learner = lrn("regr.lm")
  res = ResamplingNestedCV$new()
  res$param_set$values$folds = 5

  rr = resample(task, learner, res)

  ci_nested_cv(rr)

  loss = mlr3measures::se

  lapply(rr$predictions(), function(x) loss(x$truth, x$response))

}
