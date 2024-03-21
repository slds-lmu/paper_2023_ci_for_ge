#' @export
ResamplingAusternZhou = R6Class("ResamplingAusternZhou",
  inherit = Resampling,
  public = list(
    initialize = function(id = "austern_zhou") {
      param_set = ps(
        folds = p_int(lower = 2L, tags = "required"),
        repeats = p_int(lower = 1L, tags = "required")
      )
      param_set$values = list(folds = 10L, repeats = 1L)

      super$initialize(id = id, param_set = param_set, label = "Austern and Zhou")
    }
  ),
  active = list(
    # Here the iterations depend on the size of the task and must be set manually
    iters = function() {
      pv = self$param_set$get_values()
      (self$task_nrow %/% 2 + 2) * pv$folds * pv$repeats
    }
  ),
  private = list(
    .sample = function(ids, task, ...) {
      pv = self$param_set$get_values()
      folds = pv$folds
      repeats = pv$repeats

      if (length(ids) %% 2) {
        ids = ids[-sample(length(ids), 1)]
      }

      ids = shuffle(ids)

      left_half = ids[seq(1, length(ids) %/% 2)]
      right_half = ids[seq(length(ids) %/% 2 + 1, length(ids))]

      task_left_half = task$clone(deep = TRUE)$filter(left_half)

      list(
        left_half = left_half,
        right_half = right_half,
        cv_left_half = rsmp("repeated_cv", repeats = repeats, folds = folds)$instantiate(task_left_half),
        cv_full = rsmp("repeated_cv", folds = folds, repeats = repeats)$instantiate(task = task)
      )
    },
    .get_train = function(i) {
      pv = self$param_set$get_values()
      folds = pv$folds
      repeats = pv$repeats

      if (i <= repeats * folds) {
        return(get_private(self$instance$cv_full)$.get_train(i))
      } else if (i <= 2 * repeats * folds) {
        return(get_private(self$instance$cv_left_half)$.get_train(i - repeats * folds))
      }

      i = i - 2 * repeats * folds

      # which id is being replaced
      repl_iter = (i - 1) %/% (repeats * folds) + 1
      # print(sprintf("repl iter is %i", repl_iter))
      # the iteration of the repeated cv
      inner_iter = i - (repl_iter - 1) * repeats * folds
      # print(sprintf("inner iter is %i", inner_iter))

      ids = get_private(self$instance$cv_left_half)$.get_train(inner_iter)

      old = self$instance$left_half[repl_iter]
      new = self$instance$right_half[repl_iter]

      ids[ids == old] = new
      ids
    },
    .get_test = function(i) {
      pv = self$param_set$get_values()
      folds = pv$folds
      repeats = pv$repeats

      if (i <= repeats * folds) {
        return(get_private(self$instance$cv_full)$.get_test(i))
      } else if (i <= 2 * repeats * folds) {
        return(get_private(self$instance$cv_left_half)$.get_test(i - repeats * folds))
      }

      i = i - 2 * repeats * folds

      # which id is being replaced
      repl_iter = (i - 1) %/% (repeats * folds) + 1
      # print(sprintf("repl iter is %i", repl_iter))
      # the iteration of the repeated cv
      inner_iter = i - (repl_iter - 1) * repeats * folds
      # print(sprintf("inner iter is %i", inner_iter))

      ids = get_private(self$instance$cv_left_half)$.get_test(inner_iter)

      old = self$instance$left_half[repl_iter]
      new = self$instance$right_half[repl_iter]

      ids[ids == old] = new
      ids
    },
    .combine = function(instances) {
      .NotYetImplemented()
    }
  )
)


#' @include zzz.R
custom_resamplings[["austern_zhou"]] = function() ResamplingAusternZhou$new()
