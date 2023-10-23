#' @export
infer_simple = function(x, alpha, ...) {
  UseMethod("infer_simple")
}

#' @export
infer_simple.loss_table = function(x, alpha = 0.05, loss, ...) {
  iters = max(x$iter)

  l = lapply(seq_len(iters), function(i) {
    ci = infer_holdout(x[get("iter") == i, ], alpha, loss)
  })


  dt = rbindlist(l)

  as.data.table(as.list(colMeans(dt)))
}

#' @export
infer_simple.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, predict_set = "test", ...) { #nolint
  assert_true(inherits(x$resampling, "ResamplingCV") || inherits(x$resampling, "ResamplingRepeatedCV"))

  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions(predict_set), loss_fn)

  infer_simple(loss_table, alpha = alpha, loss = names(loss_fn))
}
