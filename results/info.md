Overview of the folders:

**raw**

This folder contains all raw data that is not processed by any files in this repository.
These files need to be downloaded from the zotero link given in the paper.

**ablation**

This folder contains the results of the parameter investigation of the well-performing inference methods:
For each method an aggregated and unaggregated file exists.

To recreate those files, first download the files into the `raw` folder and then run `make process`



* az.rds is the result of experiments/az.R which is the experiment to investigate the missing correction factor for the Austern & Zhou method.
* ci_aggr.rds is the result of ./analysis/processing/main
* ci_conz.rds is the result of running the experiments for the conservative z method again because of a bug
  in the initial experiments. It is used in ./analysis/processing/main
* good_losses.rds: the well-performing methods were also evaluated on more losses (winsorized se etc.)
* runtime.rds: the results of experiments/runtime
* sds.rds: the standard deviations of the target distributions in the datasets.
* small_cheap.rds: the conz and ncv method were applied in cheaper variants to all datasets. These are the results. Is used in analysis/processing/main
* truth_losses.rds: Because we later evaluated more loss functions this contains the true risks for all loss functions.
* truth
