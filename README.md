# Confidence Intervals for the Generalization Error

The package structure is as follows:

* `./inferGE` contains the R package that implements the methods and tasks.
* `./experiments` contains the scripts used to run the experiments


## Benchmarking Suite

The benchmarking suite is available on OpenML as a collection with [ID 396](https://www.openml.org/s/396).

## Reproducing the Experiments




1. Install the `renv` package and restore the computational environment using `renv::restore()`.
1. Adjust the configuration: To be able to run the experiments on a different cluster, the following options have to be adjusted:
   *

   * Adjust the batchtools configuration in `batchtools.conf.R`, to your cluster.
     This includes changing the template `slurm_wyoming.tmpl`.
     For more information see the [batchtools documentation](https://mllg.github.io/batchtools/articles/batchtools.html).
1. In the `.Renviro`
