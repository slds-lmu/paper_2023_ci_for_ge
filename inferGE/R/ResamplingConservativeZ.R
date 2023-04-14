#' @title Resampling for Conservative Z Test
#' @description
#' The conservative z-test requires a certain resampling.
#'
#'
#' @section Parameters:
#' * `repeats` (`integer(1)`)\cr
#'   Number of repetitions.
#'   Is initialized to 30.
#' * `ratio` :: (`integer(1)`)\cr
#'   Ratio of observations to put into the training set.
#'   Is intialized to 2/3.
#' * `M` :: (`integer(1)`)\cr
#'   The number of repetitions to estimate the variance.
#'
#' @references
#' `r format_bib("nadeau1999inference")`
#'
#' @export
ResamplingConservativeZ = R6Class("ResamplingConservativeZ",
  inherit = Resampling,
  public = list(
    initialize = function() {
      param_set = ps(
        ratio = p_dbl(0, 1, tags = "required"),
        repeats = p_int(1, tags = "required"),
        M = p_int(tags = "required")
      )
      super$initialize()
      self$param_set$values = list()
    }
  ), 
  active = list(
    iters = function(rhs) {
      assert_ro_binding(rhs)
      self$param_set$values$repeats * self$param_set$values$M
    }
  ),
  private = list(

  )
)
