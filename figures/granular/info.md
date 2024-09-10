* The figures in this folder show the different CI methods on a very granular level:
  For a given size, loss, inducer and target of inference methods, the relative coverage frequency and the width are shown.
* The width is standardized by the standard deviation of the corresponding point estimates.
  Therefore this means that the widths between methods are not compareable in absolute terms when two methods don't have the same point estimate.
  The width can only be used to judge whether an inference method is well callibrated.
  With assumption of normality (which might not hold) we roughly expect a standardized CI width of around 2 (2 * 1.96).
* The number in brackets after the method abbreviation are the total number of resampling iterations needed for the inference method.
* The plots in the 'standard_losses' folder show the squared-error and zero-one loss.
* The graphs in 'other_losses' show each loss separately, i.e. regression and classification DGPs are also shown separately.
  Note that only a subset of the methods was also evaluated on those losses in order to save computation time.

The code that creates these figures is located in `./figures/granular/` and can be adjusted.
