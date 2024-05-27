# Confidence Intervals for the Generalization Error

## Start App

To start the shiny-app, first make sure that `renv` is installed and run `renv::restore("shiny_app")` from the root of this repository.
To start the app, simply run the code below from the root of this repository.

```bash
make shiny
```

## Overview

This is the code accompanying the Paper <TODO></TODO>

The content of this repository is:

* `./shiny_app/` contains the code for the shiny-app, which allows for a convenient visual exploration of the experiment results.
* `./data/` contains some input data and is the folder where datasets that are generated will be stored.
* `./datamodels/` contains the code related to the generation of the datasets.
* `./experiments` contains the code for the main experiments comparing the different confidence interval methods.
* `./inferGE/` is the R package that implements confidence interval methods that are being compared
* `./misc/` contains some miscellaneous helper functions
* `./renv/` and `renv.lock` are for the reproducible R environment
* `./results/` contains the final results (such as figures, tables) included in the paper


## Reproducibility

The instructions to reproduce the experiments are separated into:
1. the dataset generation, see `./datamodels/README.md`. Note that the resulting datasets are also made available on
   [OpenML](https://openml.org), so the main experiments can be reproduced without this step.
1. the main experiments, see `./experiments/README.md`.
