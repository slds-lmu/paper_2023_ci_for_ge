#' @export
ResamplingBootstrapCCV = R6Class("ResamplingBootstrapCCV",
  inherit = Resampling,
  public = list(
    initialize = function() {
      param_set = ps(
        ratio = p_dbl(0, upper = 1, tags = "required"),
        repeats = p_int(1L, tags = "required")
      )
      param_set$values$ratio = 1

      super$initialize(
        id = "bootstrap_ccv",
        param_set = param_set,
        label = "Bootstrap Case Cross-Validation",
        man = "mlr3::mlr_resamplings_bootstrap_ccv"
      )
    }
  ),
  private = list(
    .sample = function(ids, task = NULL, ...) {
      pv = self$param_set$get_values()

      if (is.null(task)) {
        # This happens when grouping
        # Needs to be fixed in mlr3: https://github.com/mlr-org/mlr3/pull/901
        stratified = FALSE
      } else {
        stratified = !is.null(task$strata)
      }
      nr = round(length(ids) * pv$ratio)
      assert_true(nr > 0L)

      x = factor(seq_along(ids))
      M = replicate(pv$repeats, table(sample(x, nr, replace = TRUE)), simplify = "array")
      M = matrix(M, nrow = length(ids), ncol = pv$repeats)
      rownames(M) = NULL

      if (stratified) {
        # Here, we calculate the information holdout and bootstrap_repeat after we combined the M, because otherwise
        # we cannot check whether there are degenerate bootstrap cases
        list(row_ids = ids, M = M)
      } else {
        private$.preprocess(ids, M)
      }
    },
    .preprocess = function(ids, M) {
      holdout = map(seq_len(ncol(M)), function(i) {
        ii = M[, i] != 0
        if (sum(ii) <= 1) {
          # We would either have no test case or no train case
          return(integer(0))
        }
        which(ii)
      })
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
      # Here we need to make sture that we have no degenerate bootstrap iterations (no test or no train data).
      # These are simply dropped
      row_ids = do.call(c, map(instances, "row_ids"))
      M = do.call(rbind, map(instances, "M"))
      private$.preprocess(row_ids, M)
    }
  ),
  active = list(
    #' @field iters (`integer(1)`)\cr
    #'   The number of iterations.
    iters = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$instance)) {
        return(NA_integer_)
      }
      length(self$instance$holdout)
    }
  )
)

#' @include zzz.R
custom_resamplings[["bootstrap_ccv"]] = function() ResamplingBootstrapCCV$new()
