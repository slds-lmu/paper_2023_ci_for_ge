#' @title Calculate pairwise Loss fnuctoins
#' @description
#' Evaluate one or more pairwise loss functions on a list of predictions.
#' @param predictions (list of [`Prediction`]s)\cr
#'   A list containing the predictions.
#' @param loss_fns (named `list()` of loss `function`s)\cr
#'  The (pairwise) loss functions that are to be calculated.
#'  The names of the functions will be the names in the returned loss table.
#' @return [`data.table`]
#' @export
calculate_loss = function(predictions, loss_fns = NULL, na_value = NaN, task, resampling = NULL, iters = seq_along(predictions)) {
  assert_list(predictions, types = "Prediction")
  assert_list(loss_fns, types = "function", names = "unique", null.ok = TRUE)
  task_type = predictions[[1L]]$task_type
  loss_fns = loss_fns %??% default_loss_fn(task_type)
  assert_true(length(predictions) == length(iters))
  assert_true(is.null(resampling) || resampling$is_instantiated)

  if ("iter" %in% names(loss_fns) || "row_id" %in% names(loss_fns)) {
    stopf("Names 'iter' and 'row_id' are reserved, choose different names for the loss function.")
  }

  wrapper = if (task_type == "classif") {
    function(prediction, train_set) {
      l = lapply(loss_fns, function(loss_fn) {
        do.call(loss_fn, args = list(
          truth = prediction$truth,
          response = prediction$response,
          prob = prediction$prob,
          task = task,
          train_set = train_set
          ))
        })
      dt = as.data.table(l)
      dt$row_id = prediction$row_ids

      return(dt)
    }
  } else {
    function(prediction, train_set) {
      l = lapply(loss_fns, function(loss_fn) {
        do.call(loss_fn, args = list(
          truth = prediction$truth,
          response = prediction$response,
          se = prediction$se,
          task = task,
          train_set = train_set
          ))
      })
      dt = as.data.table(l)
      dt$row_id = prediction$row_ids

      return(dt)
    }
  }

  losses = lapply(seq_along(predictions), function(i) {
    train_set = if (!is.null(resampling)) resampling$train_set(iters[i]) else NULL
    wrapper(predictions[[i]], train_set = train_set)
  })
  dt = rbindlist(losses)

  iter = rep(seq_along(predictions), times = map_int(predictions, function(p) length(p$row_ids)))

  dt$iter = iter

  setkeyv(dt, c("iter", "row_id"))

  class(dt) = c("loss_table", class(dt))

  return(dt)
}
