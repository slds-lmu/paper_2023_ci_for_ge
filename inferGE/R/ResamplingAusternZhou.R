#' @export
ResamplingAusternZhou = R6Class("ResamplingAusternZhou",
  inherit = Resampling,
  public = list(
    initialize = function(id = "austern_zhou") {
      param_set = ps(
        folds = p_int(lower = 2L)
      )
      param_set$values = list(folds = 10L)

      super$initialize(id = id, param_set = param_set, label = "Austern and Zhou")
    },
    instantiate = function(task) {
      super$instantiate(task)
    }
  ),
  active = list(
    # Here the iterations depend on the size of the task and must be set manually
    iters = function() {
      (self$task_nrow %/% 2 + 2) * self$param_set$values$folds
    }
  ),
  private = list(
    .sample = function(ids, task, ...) {
      if (length(ids) %/% 2) {
        ignored_id = sample(length(ids), 1)
        ids  = ids[-ignored_id]
      } else {
        ignored_id = NULL
      }
      ids = shuffle(ids)

      row_ids = ids[seq(1, length(ids) %/% 2)]
      replace_ids = ids[seq(length(ids) %/% 2 + 1, length(ids))]

      tbl = data.table(
        row_id = row_ids,
        fold = shuffle(seq_along0(row_ids) %% as.integer(self$param_set$values$folds) + 1L),
        key = "fold"
      )
      list(
        tbl = tbl,
        replace_ids = replace_ids,
        ignored_id = ignored_id,
        cv_full = rsmp("cv", folds = self$param_set$values$folds)$instantiate(task = task)
      )
    },
    .get_replace = function(i) {
      # i = id - 2 * folds
      if (is.null(self$task_nrow)) stop("task_nrow must be set")
      (i - 1) %/% (self$param_set$values$folds) + 1
    },
    .get_fold = function(id) {
      if (is.null(self$task_nrow)) stop("task_nrow must be set")
      (id - 1) %% self$param_set$values$folds + 1
    },
    .get_train = function(i) {
      # if named to fold, data.table's NSE interfers with my intentions ...
      fld = private$.get_fold(i)

      if (i <= self$param_set$values$folds) {
        # first iter is the full CV
        return(get_private(self$instance$cv_full)$.get_train(i))
      } else if (i <= 2 * self$param_set$values$folds) {
        # second case is the "normal" half CV
        tbl = self$instance$tbl
      } else {
        i = i - 2 * self$param_set$values$folds
        # in all other cases we replace a value
        # we have n / 2 replace ids and ii_replace indicates the replace_ids that will be used to replace the
        # ii_replace value of row_ids
        ii_replace = private$.get_replace(i)

        tbl = self$instance$tbl
        tbl[ii_replace, "row_id"] = self$instance$replace_ids[ii_replace]
      }

      tbl[!list(fld), "row_id", on = "fold"][[1L]]
    },
    .get_test = function(i) {
      # if named to fold, data.table's NSE interfers with my intentions ...
      fld = private$.get_fold(i)

      if (i <= self$param_set$values$folds) {
        # first iter is the full CV
        return(get_private(self$instance$cv_full)$.get_train(i))
      } else if (i <= 2 * self$param_set$values$folds) {
        # second case is the "normal" half CV
        tbl = self$instance$tbl
      } else {
        i = i - 2 * self$param_set$values$folds
        # in all other cases we replace a value
        # we have n / 2 replace ids and ii_replace indicates the replace_ids that will be used to replace the
        # ii_replace value of row_ids
        ii_replace = private$.get_replace(i)

        tbl = self$instance$tbl
        tbl[ii_replace, "row_id"] = self$instance$replace_ids[ii_replace]
      }

      tbl[list(fld), "row_id", on = "fold"][[1L]]
    },
    .combine = function(instances) {
      .NotYetImplemented()
    }
  )
)


#' @include zzz.R
custom_resamplings[["austern_zhou"]] = function() ResamplingAusternZhou$new()
