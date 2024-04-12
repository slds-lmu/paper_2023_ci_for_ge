# Confidence Intervals for the Generalization Error


* `./data/` contains the data
* `./datamodels/` contains the code related to the generation of the datasets
* `./experiments` contains the code for the main experiments to evaluate the CI methods
* `./inferGE` contains the R package that implements the CI methods that are being compared
* `./misc/` contains some miscellaneous helper functions
* `./renv/` and `renv.lock` are for the reproducible R environment
* `./results/` contains the final results (such as figures, tables) included in the paper

For instructions on how to reproduce
1. the dataset generation, see `./datamodels/README.md`. Note that the resulting datasets are also made available on
   [OpenML](https://openml.org), so the main experiments can be reproduced without this step.
1. the main experiments, see `./experiments/README.md`.

