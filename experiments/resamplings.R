# here we have two sublists:
# * both: resamplings are applies to both the small and large setting
# * small: resamplings that are only applied in the small setting
#
# TODO: We need to check that all methods are covered by that:
# There are more methods than entries here (multiple methods for CV e.g.)
# For every resampling we do not only make predictions on the test set, bu also on the holdout set
resampling_ids = list(both = list(
  #holdout is the same as subsampling with 1 repetition
  holdout           = list(id = "holdout", params = list()),
  subsampling_10    = list(id = "subsampling", params = list(repeats = 10)),
  subsampling_50    = list(id = "subsampling", params = list(repeats = 30)),
  subsampling_100   = list(id = "subsampling", params = list(repeats = 30)),
  cv_10             = list(id = "cv", params = list(folds = 10)),
  repeated_cv_5_10  = list(id = "repeated_cv", params = list()),
  repeated_cv_10_10 = list(id = "repeated_cv", params = list()),
  nested_cv         = list(id = "nested_cv", params = list("TODO")),
  nadeau            = list("TODO"), # They use subsampling I believe
  diettrich         = list(id = "repeated_cv", params = list(repeats = 5, folds = 2)),
  # gives the true prediction error
  prediction_error  = list(id = "holdout", params = list(ratio = 1))
  ), small = list(
  loo               = list(id = "loo", params = list()),
  jiang             = list(id = "bootstrap_ccv"),
  )
)

# takes in a resampling id and creates the resamplin
make_resampling = function(x) {
  do.call(rsmp, args = c(list(id = x$id), x$params))
}
