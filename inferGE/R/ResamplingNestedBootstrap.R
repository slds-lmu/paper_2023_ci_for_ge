#' @export
ResamplingNestedBootstrap = R6Class("ResamplingNestedBootstrap",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function() {
      param_set = ps(
        reps_outer = p_int(lower = 1, tags = "required"),
        reps_inner = p_int(lower = 1, tags = "required")
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
      if (i <= pv$reps_outer) stopf("too small")
      i = i - pv$reps_outer
      rep_outer = ceiling(i / pv$reps_inner)
      rep_inner = i - (rep_outer - 1) * pv$reps_inner
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
      pv$reps_outer * (pv$reps_inner + 1L)
    }
  ),
  private = list(
    .sample = function(ids, task, ...) {
      task = task$clone(deep = TRUE)
      pv = self$param_set$get_values()

      outer_boot = rsmp("bootstrap", repeats = pv$reps_outer, ratio = 1)$instantiate(task)

      resamplings = map(seq_len(outer_boot$iters), function(i) {
        ids = outer_boot$train_set(i)
        task$row_roles$use = ids

        list(
          rsmp("insample")$instantiate(task),
          rsmp("bootstrap", ratio = 1, repeats = pv$reps_inner)$instantiate(task)
        )
      })

      insamples = map(resamplings, 1L)
      bootstraps = map(resamplings, 2L)

      list(
        insample = insamples,
        bootstrap = bootstraps
      )
    },
    .get_train = function(i) {
      pv = self$param_set$get_values()
      if (i <= pv$reps_outer) {
        return(get_private(self$instance$insample[[i]])$.get_train(1))
      }
      info = self$unflatten(i)
      get_private(self$instance$bootstrap[[info$rep_outer]])$.get_train(info$rep_inner)
    },
    .get_test = function(i) {
      pv = self$param_set$get_values()
      if (i <= pv$reps_outer) {
        return(get_private(self$instance$insample[[i]])$.get_test(1))
      }
      info = self$unflatten(i)
      get_private(self$instance$bootstrap[[info$rep_outer]])$.get_test(info$rep_inner)
    }
  )
)

#' @include zzz.R
custom_resamplings[["nested_bootstrap"]] = function() ResamplingNestedBootstrap$new()
