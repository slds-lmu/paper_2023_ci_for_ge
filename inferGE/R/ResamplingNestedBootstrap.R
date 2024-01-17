#' @export
ResamplingNestedBootstrap = R6Class("ResamplingNestedBootstrap",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function() {
      param_set = ps(
        ratio = p_dbl(lower = 0, upper = 1, tags = "required"),
        reps_outer = p_int(lower = 1, tags = "required"),
        reps_inner = p_int(lower = 1, tags = "required")
      )
      param_set$set_values(
        ratio = 1
      )

      super$initialize(
        id = "nested_bootstrap",
        param_set = param_set,
        label = "Nested Bootstrap",
        man = "mlr3::mlr_resamplings_nested_bootstrap"
      )
    },
    flatten = function(rep_outer, rep_inner) {
      pv = self$param_set$get_values()

      pv$reps_inner * (rep_outer - 1) + rep_inner
    },
    unflatten = function(i) {
      pv = self$param_set$get_values()
      rep_outer = ceiling(i / pv$reps_inner)
      rep_inner = i - (rep_outer - 1) * pv$reps_outer
      list(
        rep_outer = rep_outer,
        rep_inner = rep_inner
      )
    }
  ),
  active = list(
    iters = function(rhs) {
      assert_ro_binding(rhs)

      pv = self$param_set$get_values()
      pv$reps_outer * pv$reps_inner
    }
  ),
  private = list(
    .sample = function(ids, ...) {
      pv = self$param_set$get_values()

      map(seq_len(pv$reps_outer), function(o) {
        rsmp("bootstrap", ratio = pv$ratio, repeats = pv$reps_inner)
      })
    },
    .get_train = function(i) {
      info = self$unflatten(i)
      get_private(self$instance[[info$rep_outer]])$.get_train(info$rep_inner)
    },
    .get_test = function(i) {
      info = self$unflatten(i)
      get_private(self$instance[[info$rep_outer]])$.get_test(info$rep_inner)
    },
    .combine = function(instances) {
      pv = self$param_set$get_values()
      instances = transpose_list(instances)
      browser()
      map(instances, function(insts) {
        private(insts[[1L]])$.combine(insts)
      })
    }
  )
)

#' @include zzz.R
custom_resamplings[["nested_bootstrap"]] = function() ResamplingNestedBootstrap$new()
