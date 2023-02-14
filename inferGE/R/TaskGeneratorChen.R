#' @title Regression Task Generator for Chen Model
#'
#' @description
#' Creates a task generator for the Chen model.
#'
#' TODO: Add reference when it is available in the overleaf.
#'
#' @export
TaskGeneratorChen = R6Class("TaskGeneratorChen",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        M = p_int(lower = 1L, tags = "required")
      )
      super$initialize(
        id = "chen",
        task_type = "regr",
        label = "Chen Model",
        param_set = param_set,
        man = "mlr3::mlr_task_generators_chen"
      )
    }
  ),
  private = list(
    .generate = function(n) {
      pv = self$param_set$get_values()
      v = lapply(1:6, function(i) runif(n))
      y = 0.25 * exp(4 * v[[1L]]) + 4 / (1 + exp(-20 * (v[[2L]] - 0.5))) + 3 * v[[3L]] + rnorm(n, sd = 0.2)

      # generate the features
      xs = map(1:6, function(j) {
        map(seq_len(pv$M), function(m) {
          v[[j]] + (0.01 + (0.5 * (m - 1) / (pv$M - 1))) * rnorm(n, sd = 0.3)
        })
      })

      xs = unlist(xs, recursive = FALSE)
      dat = do.call(data.table, args = c(list(y), xs))
      colnames(dat) = c("y", Reduce(c, lapply(1:6, function(j) paste0("x", j, "_", seq_len(pv$M)))))
      as_task_regr(dat, target = "y", id = "chen")
    }
  )
)

#' @include zzz.R
mlr_task_generators$add("chen", function() TaskGeneratorChen$new())
