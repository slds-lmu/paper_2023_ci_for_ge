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
1. the main experiments, see `./experiments/README.md`.

## Downloading the datasets

Because of the large size of the [benchmark datasets](https://www.openml.org/search?type=study&study_type=task&id=441), it is important to download them in **parquet** format.
However, the download might still fail. In this case, simply retry until it works.
