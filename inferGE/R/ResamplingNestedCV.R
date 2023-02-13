ResamplingNestedCV = R6::R6Class("ResamplingNestedCV",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function() {
      param_set = ps(
        folds = p_int(lower = 1L, tags = "required")
      )
      super$initialize(id = "nested_cv", param_set = param_set,
        label = "Nested Cross-Validation", man = "mlr3::mlr_resamplings_nested_cv"
      )
    },
    #' @description For a given iteration return info about the inner and outer loop.
    #' In case the iteration belongs to the outer loop, the value `inner` is set to `NA`.
    #' @param iter (`integer(1)`)\cr
    #'   The iteration.
    iter_to_outer_inner = function(iter) {
      assert_int(iter, lower = 1L, upper = self$iters)
      folds = self$param_set$get_values()$folds
      if (iter <= folds) {
        list(
          outer = iter,
          inner = NA_integer_
        )
      } else {
        list(
          outer = ceiling((iter - folds) / (folds - 1)),
          inner = (iter - folds - 1) %% (folds - 1) + 1
        )
      }
    },
    #' @description Obtain the iteration for the specified `(outer, inner)` tuple.
    #' If `inner` is missing, the outer iteration is returned.
    outer_inner_to_iter = function(outer, inner = NULL) {
      folds = self$param_set$get_values()$folds
      assert_int(outer, lower = 1L, upper = folds)
      assert_int(inner, lower = 1L, upper = folds - 1, null.ok = TRUE)

      if (is.null(inner)) {
        outer
      } else {
        folds + (outer - 1) * (folds - 1) + inner
      }
    }
  ),
  active = list(
    iters = function(rhs) {
      assert_ro_binding(rhs)
      self$param_set$values$folds^2
    }
  ),
  private = list(
    .sample = function(ids, ...) {
      folds = self$param_set$get_values()$folds
      data.table(
        row_id = ids,
        fold = shuffle(seq_along0(ids) %% as.integer(folds) + 1L),
        key = "fold"
      )
    },
    .get_train = function(i) {
      folds = self$param_set$values$folds
      if (i <= folds) {
        # This is an outer fold
        self$instance[!list(i), "row_id", on = "fold"][[1L]]
      } else {
        info = self$iter_to_outer_inner(i)
        folds_inner = seq_len(folds)[-info$outer]

        self$instance[get("fold") %nin% c(info$outer, folds_inner[info$inner]), "row_id"][[1L]]
      }
    },
    .get_test = function(i) {
      folds = self$param_set$values$folds
      if (i <= folds) {
        # This is an outer fold
        self$instance[list(i), "row_id", on = "fold"][[1L]]
      } else {
        info = self$iter_to_outer_inner(i)
        folds_inner = seq_len(folds)[-info$outer]
        self$instance[list(folds_inner[info$inner]), "row_id", on = "fold"][[1L]]
      }
    },
    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    }
  )
)


