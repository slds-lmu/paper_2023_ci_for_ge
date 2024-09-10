# Constructing CIs for 'the'  Generalization Error

This is the code to reproduce the experiments from the Paper: Constructing CIs for 'the' Generalization Error.

Link to Zotero raw data: TODO

The content of this repository is:

* `./analysis/` contains the code to process the results and to reproduce all the figures.
* `./data/` contains some input data and is the folder where generated datasets are stored.
* `./datamodels/` contains the code related to the generation of the datasets.
* `./experiments` contains the code for the main experiments to investigate the inference methods.
* `./figures` is the folder where the figures are stored.
  Only the figures from the main paper are stored here, some additional l
* `./inferGE/` is the R package that implements confidence interval methods that are being compared.
   This is research code.
   If you want to use the well-performing inference methods in R use this repository: https://github.com/mlr-org/mlr3inferr.
* `./renv/` and `renv.lock` are for the reproducible R environment
* `./results/` contains the final results (such as figures, tables) included in the paper

## Reproducibility

The instructions to reproduce the experiments are separated into:
1. the dataset generation, see `./datamodels/README.md`. Note that the resulting datasets are also made available on
   [OpenML](https://openml.org), so the main experiments can be reproduced without this step.
   Note: The code and instructions to reproduce the results are still being cleaned up.
1. the main experiments, see `./experiments/README.md`.
   Note: The code and instructions to reproduce the results are still being cleaned up.

## Downloading the datasets

Because of the large size of the [benchmark datasets](https://www.openml.org/search?type=study&study_type=task&id=441), it is important to download them in **parquet** format.
However, the download might still fail. In this case, simply retry until it works.


## Extending the experiment

In order to evaluate new inference methods, the following steps need to be followed:

1. In case the inference method requires a new resampling method that is not yet implemented in `mlr3`, you need to implement a new `Resampling` class, e.g. by adding it to the `inferGE` R package in the folder with the same name.
   For an example, e.g. see `ResamplingNestedCV`.
1. Implement the inference method itself, e.g. in the `inferGE` packages.
   As an example see the `infer_bates.R` file which uses the resample result ccreated by `ResamplingNestedCV`.
1. Add the resampling method to the experiment definition from `./experiments/resample`.
1. Add the inferece method to the definitions from `./experiments/ci.R`
1. Run the resample experiment and then the CIs.

Don't hesitate to contact us if you want to reuse this code!

## Converting the Files to another format

If you don't want to work with R but still work with the results via e.g. python, you can achieve this by:

1. Starting the `R` interpreter
2. Read in the relevant `.rds` file using `readRDS(<path>)` 
3. Write the data e.g. to CSV using the `write.csv` function.
