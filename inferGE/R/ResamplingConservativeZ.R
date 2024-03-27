#' @export
ResamplingConservativeZ = R6Class("ResamplingConservativeZ",
  inherit = mlr3::Resampling,
  public = list(
    initialize = function() {
      param_set = ps(
        J = p_int(lower = 1L, tags = "required"),
        M = p_int(lower = 1L, tags = "required"),
        ratio   = p_dbl(2/3, 1, tags = "required")
      )
      super$initialize(
        id = "conservative_z",
        param_set = param_set,
        label = "Conservative Z",
        man = "mlr3::mlr_resamplings_conservative_z"
      )
    }
  ),
  private = list(
    .sample = function(ids, task, ...) {
      dots = list(...)
      pv = self$param_set$get_values()
      J = pv$J
      M = pv$M
      ratio = pv$ratio

      instance = list(
        subsampling = rsmp("subsampling", repeats = J, ratio = ratio)$instantiate(task)
      )

      n_task = task$nrow

      n1 = round(n_task * ratio)
      n2 = n_task - n1

      # each subsampling of the paired subsampling is applied to datasets of size n_sub
      n_sub = (n1 - n1 %% 2) / 2

      task1 = task$clone(deep = TRUE)
      task2 = task$clone(deep = TRUE)

      subsamplings_variance = list()

      # we want the same n2 for the subsets, just the new n1' is smaller than the n1 above
      # n1' = round(n_sub * new_ratio)
      # n2 + n1' = n_sub
      # n2 = n_sub - round(n_sub * new_ratio)
      # n_sub - n_2 = round(n_sub * new_ratio)
      # new_ratio = (n_sub - n_2 ) / n_sub + rounding

      new_ratio = (n_sub - n2) / n_sub

      for (m in seq_len(M)) {
        ids = sample(task$row_roles$use, n_sub * 2)

        task1$row_roles$use = ids[seq(1, n_sub)]
        task2$row_roles$use = ids[seq(n_sub + 1, 2 * n_sub)]

        subsamplings_variance[[length(subsamplings_variance) + 1L]] = list(
          rsmp("subsampling", repeats = J, ratio = new_ratio)$instantiate(task1),
          rsmp("subsampling", repeats = J, ratio = new_ratio)$instantiate(task2)
        )
      }

      instance$subsamplings_variance = subsamplings_variance

      return(instance)
    },
    .get_info = function(i) {
      if (i <= self$param_set$values$J) {
        info = list(
          resampling = self$instance$subsampling,
          i = i
        )
      } else {
        M = self$param_set$values$M
        J = self$param_set$values$J
        total = self$iters - J

        pair_idx = (i - J - 1) %/% (2 * J) + 1

        reminder = i - J - (pair_idx - 1) * 2 * J
        if (reminder <= J) {
          i = reminder
          j = 1
        } else {
          i = reminder - J
          j = 2
        }

        info = list(
          resampling = self$instance$subsamplings_variance[[pair_idx]][[j]],
          i = i
        )
      }

      return(info)
    },
    .get_train = function(i) {
      info = private$.get_info(i)
      get_private(info$resampling)$.get_train(info$i)
    },

    .get_test = function(i) {
      info = private$.get_info(i)
      get_private(info$resampling)$.get_test(info$i)
    }
  ),
  active = list(
    iters = function(rhs) {
      self$param_set$values$J + 2 * self$param_set$values$M * self$param_set$values$J
    }
  )
)

#' @include zzz.R
custom_resamplings[["conservative_z"]] = ResamplingConservativeZ
