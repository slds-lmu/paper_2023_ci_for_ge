#' @title Bates Classification Task Generator
#'
#' @name mlr_task_generators_bates_regr
#'
#' @description
#' A [mlr3::TaskGenerator] for the bates task.
#'
#' @export
TaskGeneratorBatesClassif = R6::R6Class("TaskGeneratorBatesClassif",
  inherit = mlr3::TaskGenerator,
  public = list(
    #' @description
    #' creates a new instance of this [r6][r6::r6class] class.
    initialize = function() {
      param_set = ps(
        ones = p_int(lower = 1, tags = "required"),
        zeros = p_int(lower = 0, tags = "required"),
        c = p_dbl(lower = 0, tags = "required")
      )

      super$initialize(id = "classif.bates", "classif", character(), param_set = param_set,
        label = "Bates Classification", man = "mlr3::mlr_task_generators_bates_classif")
    }
  ),

  private = list(
    .generate = function(n) {
      pv = self$param_set$get_values()

      X1 = matrix(rnorm(pv$ones * n), nrow = n)
      scores = pv$c * rowSums(X1)
      p = 1 / (1 + exp(-scores))
      y = map_chr(p, function(prob) sample(c("0", "1"), 1L, prob = c(1 - prob, prob)))

      X0 = matrix(rnorm(pv$zeros * n), nrow = n)
      nms = c("y", paste0("important", seq_len(pv$ones)), paste0("unimportant", seq_len(pv$zeros)))
      dat = set_names(data.table(y, X1, X0), nms)
      as_task_classif(dat, target = "y", id = "classif.bates")
    }
  )
)

#' @title Bates Regression Task Generator
#'
#' @name mlr_task_generators_bates_classif
#'
#' @description
#' A [mlr3::TaskGenerator] for the bates task.
#'
#' @export
TaskGeneratorBatesRegr = R6::R6Class("TaskGeneratorBatesRegr",
  inherit = mlr3::TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        ones = p_int(lower = 1L, tags = "required"),
        zeros = p_int(lower = 0, tags = "required"),
        c = p_dbl(lower = 0, tags = "required")
      )

      super$initialize(id = "regr.bates", "regr", character(), param_set = param_set,
        label = "Bates Regression", man = "mlr3::mlr_task_generators_bates_regr")
    }
  ),

  private = list(
    .generate = function(n) {
      pv = self$param_set$get_values()

      X1 = matrix(rnorm(pv$ones * n), nrow = n)
      y = pv$c * rowSums(X1) + rnorm(n)

      X0 = matrix(rnorm(pv$zeros * n), nrow = n)

      nms = c("y", paste0("important", seq_len(pv$ones)), paste0("unimportant", seq_len(pv$zeros)))
      dat = set_names(data.table(y, X1, X0), nms)
      as_task_regr(dat, target = "y", id = "regr.bates")
    }
  )
)

#' @include zzz.R
mlr_task_generators$add("classif.bates", function() TaskGeneratorBatesClassif$new())

# #' @include zzz.R
mlr_task_generators$add("regr.bates", function() TaskGeneratorBatesRegr$new())
