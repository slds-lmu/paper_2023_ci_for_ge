EPS = 0.0000001
assert_classif = getFromNamespace("assert_classif", ns = "mlr3measures")
assert_regr = getFromNamespace("assert_regr", ns = "mlr3measures")
assert_binary = getFromNamespace("assert_binary", ns = "mlr3measures")

.se = function(truth, response) {
  (truth - response)^2
}

#' @export
percentual_se = function(truth, response, ...) {
  assert_regr(truth, response = response)
  mlr3measures::se(truth, response) / (abs(truth) + EPS)
}

#' @export
percentual_ae = function(truth, response, ...) {
  assert_regr(truth, response = response)
  mlr3measures::ae(truth, response) / (abs(truth) + EPS)
}

#' @export
standardized_se = function(truth, response, task, train_set = NULL, ...) {
  assert_regr(truth, response = response)
  mlr3measures::se(truth, response) / (var(task$truth(train_set)) + EPS)
}

#' @export
standardized_ae = function(truth, response, task, train_set = NULL, ...) {
  assert_regr(truth, response = response)
  mlr3measures::ae(truth, response) / (sd(task$truth(train_set)) + EPS)
}

#' @export
winsorized_se = function(truth, response, ...) {
  se = mlr3measures::se(truth = truth, response = response, ...)
  upper = quantile(se, probs = 0.9)
  se[se >= upper] = upper
  se
}

#' @export
logloss = function(truth, prob, eps = 1e-15, ...) {
  assert_classif(truth, prob = prob)
  checkmate::assert_number(eps, lower = 0, upper = 1)
  ii = match(as.character(truth), colnames(prob))
  p = prob[cbind(seq_len(nrow(prob)), ii)]
  p = pmax(eps, pmin(1 - eps, p))
  -log(p)
}

#' @export
bbrier = function(truth, prob, ...) {
  # we don't really care which is the 'real' positive class, we just want one class
  positive = levels(truth)[1L]
  assert_binary(truth, prob = prob[, positive], positive = positive)
  .se(truth == positive, prob[, positive])
}

#' @export
MeasureRegrStdMSE = R6Class("MeasureRegrStdMSE",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.std_mse",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        packages = "inferGE",
        properties = "requires_task",
        label = "Standardized MSE"
      )
    }
  ),
  private = list(
    .score = function(prediction, train_set, ...) {
      mean(standardized_se(truth = prediction$truth, response = prediction$response, ...))
    }
  )
)

#' @export
MeasureRegrStdMAE = R6Class("MeasureRegrStdMAE",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.std_mae",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        packages = "inferGE",
        properties = "requires_task",
        label = "Standardized MSE"
      )
    }
  ),
  private = list(
    .score = function(prediction, train_set, ...) {
      mean(standardized_ae(truth = prediction$truth, response = prediction$response, ...))
    }
  )
)
#' @export
MeasureRegrPercentualMSE = R6Class("MeasureRegrPercentualMSE",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.percentual_se",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        packages = "inferGE",
        label = "Percentual MSE"
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      mean(percentual_se(truth = prediction$truth, response = prediction$response, ...))
    }
  )
)

#' @export
MeasureRegrPercentualMAE = R6Class("MeasureRegrPercentualMAE",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.percentual_ae",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        packages = "inferGE",
        label = "Percentual MAE"
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      mean(percentual_ae(truth = prediction$truth, response = prediction$response, ...))
    }
  )
)

#' @export
MeasureRegrWinsorizedMSE = R6Class("MeasureRegrWinsorizedMSE",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.winsorized_mse",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        packages = "inferGE",
        label = "Percentual MAE"
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      mean(winsorized_se(truth = prediction$truth, response = prediction$response, ...))
    }
  )
)

#' @include zzz.R
custom_measures[["regr.std_mse"]] = function() MeasureRegrStdMSE$new()
custom_measures[["regr.percentual_mse"]] = function() MeasureRegrPercentualMSE$new()

custom_measures[["regr.std_mae"]] = function() MeasureRegrStdMAE$new()
custom_measures[["regr.percentual_mae"]] = function() MeasureRegrPercentualMAE$new()

custom_measures[["regr.winsorized_mse"]] = function() MeasureRegrWinsorizedMSE$new()
