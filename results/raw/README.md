**Files for Further Processing**

Some files in this folder should not be worked with directly but are instead further
processed by the files in `./analysis/processing`.

Files primarily processed for the ablation analysis (`./analysis/processing/ablation.R`):

* `conz_cheap_best_orig.rds`
* `conz_cheap_orig.rds`
* `conz_orig.rds`
* `cort_best_orig.rds`
* `cort_orig.rds`
* `cv_orig.rds`
* `ho_orig.rds`
* `ncv_cheap_orig.rds`
* `ncv_orig.rds`
* `ci_for_ge.rds`
* `ci_conz.rds`
* `good_losses.rds`
* `lm.rds`
* `oob_32.rds`
* `oob.rds`
* `ridge.rds`
* `small_cheap.rds`
* `truth_loss.rds`

Don't work with these files directly as they are poorly documented.
Work with the files in `./results/main` and `./results/ablation` instead.
Both folders have another `README.md` documenting the columns.

To create these files, first download the raw files in put them into the right folder.
Also make sure to have the required packages installed (see `renv.lock`).
Then run `make process` from the root of this directory.

Other files can be worked with directly and are not further processed: 

**az.rds**

This file contains the result of `./experiments/az.R` which empirically investigates the 
correction factor for the austern & zhou method.

It contains the following columns:
* `n`: The dataset size to which the Austern & Zhou Variance Estimator is applied
* `est`: The variance estimate from the Austern & Zhou method, aggregated over 10 repetitions.
* `se_est`: The standard error for the variance estimate
* `truth`: The 'true' variance of the cross-validation, estimated using 100 indendent repetitions of cross-validation.

For more information see the appendix of the paper.


**runtime.rds**

This table contains the results for the runtime analysis from the appendix of the paper.
The columns are:

* `method`: The inference method
* `size`: The dataset size to which the inference method is applied.
* `inducer`: The inducer that was used with the inference method.
* `time`: The total runtime of the resamplings that are required for the inference method.

**sds.rds**

This file just contains the standard deviations of the target values of the full datasets (only for regression).

