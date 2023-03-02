#' @title Naive CV Estimation for 
#' @param x (any)\cr 
#'  Whether to 
#' @param alph
#' @param loss
#'
#' @export
infer_naive_cv = function(x, alpha, loss, within_folds = TRUE) {
  UseMethod("infer_naive_cv")
}

infer_naive_cv.BenchmarkResult = function(x, alpha = 0.05, loss) { #nolint
  infer_method_bmr(x, alpha, loss, infer_naive_cv)
}

infer_naive_cv.ResampleResult = function(x, alpha = 0.05, loss) {
  assert_numeric(alpha, lower = 0, upper = 1)
  loss_fn = get_loss_fn(loss, x)
  loss_table = get_loss_table(x, loss_fn)
  estimate = mean(loss_table$loss)
  se = sd(loss_table$loss) / sqrt(x$task$nrow)
  z = abs(qnorm(alpha / 2))
  halfwidth = se * z
  lower = estimate - halfwidth
  upper = estimate + halfwidth
  data.table(
    estimate = estimate, 
    lower = lower, 
    upper = upper, 
    variance = NA, 
    method = "naive"
  )
}


