#' @title Bootstrap Case Cross-Validation Percentile findInterval()
#'
#' @name mlr_resamplings_bootstrap_ccv
#'
#' @description
#' Create bootstrap samples like in [`mlr3::ResamplingBootstrap`].
#' For each of these bootstrap samples, conduct "leave one case out" (LOCO) cross-validation.
#' This implies that the training sizes in the LOCO differ.
#'
#' @section Parameters:
#' * `repeats` :: (`integer(1)`)\cr
#'   Number of repetitions.
#' * `ratio` :: (`numeric(1)`)\cr
#'   Ratio of observations to put into the training set.
#' * `retries` :: (`integer(1)`)\cr
#'   The parameter controls the number of sampling retries until an instance is sampled that has no degenerate case.
#'   The default is 10. All degenerate bootstrap repetitions are dropped, but a warning is given.
#'
#' @templateVar id bootstrap_ccv
#' @template resampling
#'
#' @references
#' `r format_bib("jiang2008")`
#' @export
#' @examples
#' res = rsmp("bootstrap_ccv", ratio = 0.5, repeats = 2, retries = 2)
#' task = tsk("penguins")
#' res$instantiate(task)
#' res
ResamplingBootstrapCVV = R6Class("ResamplingBootstrapCVV",
  inherit = Resampling,
  public = list(
    #' @description
    #' creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        ratio = p_dbl(0, upper = 1, tags = "required"),
        repeats = p_int(1L, tags = "required"),
        retries = p_int(lower = 1, default = 10)
      )

      super$initialize(
        id = "bootstrap_ccv",
        param_set = param_set,
        label = "Bootstrap Case Cross-Validation",
        man = "mlr3::mlr_resamplings_bootstrap_ccv"
      )
    }
  ),
  private = list(
    .sample = function(ids, ...) {
      pv = self$param_set$get_values()
      retries = pv$retries %??% 10

      nr = round(length(ids) * pv$ratio)
      assert_true(nr > 0L)
      x = factor(seq_along(ids))

      # Now we create the bootstrap samples and resample the degenerate columns at most `retries` times
      attempts = 0
      success = FALSE
      needed_cols = seq_len(pv$repeats)

      while (!success && attempts < retries) {
        Msub = replicate(length(needed_cols), table(sample(x, nr, replace = TRUE)), simplify = "array")
        # replicate() drops the dimension if nr = 1
        Msub = matrix(Msub, nrow = length(ids), ncol = length(needed_cols))
        rownames(Msub) = NULL
        if (attempts == 0) {
          # In the first iteration all columns are needed
          # Usuaylly also only this case is being executed
          M = Msub
        } else {
          # In case there are degenerate columns, we fill them up with a new attempt at sampling non-degenerate cols
          # needed_cols are all columns (in the first iteration) and otherwise the degenerate columns
          M[, needed_cols] = Msub
        }
        # The needed_cols are being updated to be the degenerate cols in which there is only a single case in a 
        # bootstrap sample
        needed_cols = which(colSums(M != 0) < 2)
        success = length(needed_cols) == 0
        attempts = attempts + 1
      }

      if (!success) {
        M = M[, -needed_cols, drop = FALSE]
        warningf("Still got %d out of %d degenerate bootstrap samples after %s retries. They are dropped.", 
          length(needed_cols), pv$repeats, retries)
      }

      # If we have a bootstrap sample with only one case, we cannot do the "leave one case out" because either
      # the test or the train set is empty.
      # If the train set is empty, there is a test set, but the fallback learner would be triggered

      # These are just all the indices that are non-zero. I.e. holdout[i] is the index of the id that is the holdout
      # case in iter `i`
      holdout = map(seq_len(ncol(M)), function(i) which(M[, i] != 0))
      # This indicates the bootstrap sample that is used in the i-th iteration of the resmapling
      # I.e. bootstrap_repeat[i] is the index of the id that is used in the i-th iteration
      bootstrap_repeat = unlist(lapply(seq_len(ncol(M)), function(r) rep(r, length(holdout[[r]]))))
      holdout = unlist(holdout)

      list(row_ids = ids, M = M, holdout = holdout, bootstrap_repeat = bootstrap_repeat)
    },

    .get_train = function(i) {
      holdout_idx = self$instance$holdout[i]
      bootstrap_repeat = self$instance$bootstrap_repeat[i]
      times = self$instance$M[, bootstrap_repeat, drop = TRUE]
      times[holdout_idx] = 0
      rep(self$instance$row_ids, times = times)
    },

    .get_test = function(i) {
      self$instance$row_ids[self$instance$holdout[i]]
    },

    .combine = function(instances) {
      list(
        row_ids = do.call(c, map(instances, "row_ids")),
        M = do.call(rbind, map(instances, "M")),
        holdout = do.call(c, map(instances, "holdout")),
        bootstrap_repeat = do.call(c, map(instances, "bootstrap_repeat"))
      )
    }
  ),
  active = list(
    #' @field iters (`integer(1)`)\cr
    #'   The number of iterations.
    iters = function(rhs) {
      assert_ro_binding(rhs)
      # to calculate the iters, we must have already instantiated the resampling
      if (is.null(self$instance)) {
        return(NA_integer_)
      }
      length(self$instance$holdout)
    }
  )
)

#' @include zzz.R
mlr_resamplings$add("bootstrap_ccv", function() ResamplingBootstrapCVV$new())
