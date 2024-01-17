ensure_within = function(x, lower, upper) {
  if (x < lower) {
    return(lower)
  } else if (x > upper) {
    return(upper)
  } else {
    return(x)
  }
}

infer_method_bmr = function(x, alpha, loss, method) {
  resample_results = x$resample_results$resample_result

  info = data.table(
    resample_result = resample_results,
    learner_id = map(resample_results, function(rr) rr$learner$id),
    resampling_id = map(resample_results, function(rr) rr$resampling$id),
    task_id = map(resample_results, function(rr) rr$task$id)
  )

  tbl = rbindlist(lapply(resample_results, method, loss = loss, alpha = alpha))

  cbind(info, tbl)
}

#' @param x [mlr3::ResampleResult]\cr
#'   The resample result
#' @param loss (`function()`)\cr
#'   The loss function.
get_loss_table = function(x, loss_fn) {
  preds = x$predictions()
  data.table(
    iter = unlist(map(seq_len(x$iters), function(i) rep(i, times = NROW(preds[[i]]$row_ids)))),
    # TODO: Not sure whether we need the row id
    row_id = unlist(map(seq_len(x$iters), function(i) preds[[i]]$row_ids)),
    loss = unlist(map(seq_len(x$iters), function(i) loss_fn(truth = preds[[i]]$truth, response = preds[[i]]$response)))
  )
}


get_loss_fn = function(loss = NULL, rr) {
  loss = loss %??% switch(rr$task$task_type, classif = "zero_one", regr = "se", stop())
  assert_string(loss)

  if (loss == "percentual_se") {
    assert_true(rr$task_type == "regr")
    return(percentual_se)
  } else if (loss == "standardized_se") {
    assert_true(rr$task_type == "regr")
    return(standardized_se)
  }

  loss_info = get(loss, mlr3measures::measures, inherits = FALSE)

  # FIXME: This is currently not really transparent in mlr3
  assert_false(loss_info$aggregated,
    .var.name = "The measure must allow to calculate unaggregated observation-wise losses.")

  assert_true(loss_info$type == rr$task_type)

  loss_fn = getFromNamespace(loss, ns = "mlr3measures")

  return(loss_fn)
}


default_loss_fn = function(task_type) {
  loss_fn = switch(task_type,
    classif = list(zero_one = mlr3measures::zero_one),
    regr = list(se = mlr3measures::se),
    stopf("Task type '%s' currently not supported.", task_type)
  )
}

assert_alpha = function(x) {
  assert_numeric(x, len = 1L, lower = 0, upper = 1)
}
