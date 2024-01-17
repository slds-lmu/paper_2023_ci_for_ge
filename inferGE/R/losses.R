EPS = 0.0000001

#' @export
percentual_se = function(truth, response, ...) {
  mlr3measures::se(truth, response) / (abs(truth) + EPS)
}

#' @export
standardized_se = function(truth, response, ...) {
  mlr3measures::se(truth, response) / sd(truth)
}
