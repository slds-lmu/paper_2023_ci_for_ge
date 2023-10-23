#' @title Paired T-Test with 5x2 Cross-Validation
#'
#' @description
#' Approximate t-test using 5x2 Cross-Validation.
#'
#' @param x ([`ResampleResult`] or  [`BenchmarkResult`])\cr
#'   The resample result or benchmark result using a [`ResamplingNestedCV`] as the resampling technique.
#' @template param_alpha
#' @param loss (`character(1)`)\cr
#'   The observation loss. One of `mlr3measures::measures` that calculates oberservation-wise losses in an unaggergated
#'   manner. E.g. `"se"` for the squared error or `"zero_one"` for the 0-1 loss.
#'
#' @references
#' `r format_bib("dietterich1998approximate")`
#' @export
infer_52cv = function(x, alpha = 0.05, ...) {
  UseMethod("infer_52cv")
}

#' @export
infer_52cv.loss_table = function(x, alpha = 0.05, loss, ...) {
  assert_numeric(alpha, len = 1, lower = 0, upper = 1)

  if (!test_subset(loss, colnames(x))) {
    stopf("Loss '%s' not present in loss table.", loss)
  }

  fold_name = make.unique(c(colnames(x), "fold"))[ncol(x) + 1L]
  replication_name = make.unique(c(colnames(x), "replication"))[ncol(x) + 1L]

  x[[fold_name]] = rep(1:2, times = 5)
  x[[replication_name]] = rep(1:5, each = 2)

  # first we calculate the mean over both folds for each replication
  ps = x[, list(avg = mean(get(loss))), by = c(replication_name, fold_name)]

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

#' @export
infer_52cv.ResampleResult = function(x, alpha = 0.05, loss_fn = NULL, predict_set = "test", ...) { #nolint
  if (is.null(loss_fn)) loss_fn = default_loss_fn(x$task_type)

  loss_table = calculate_loss(x$predictions(predict_set), loss_fn)

  infer_52cv(loss_table, alpha = alpha, loss = names(loss_fn))
}
