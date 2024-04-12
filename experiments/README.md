## Reproducing the Main Experiments

Due to the runtime of the experiments it is impossible to reproduce them on a
personal computer unless you want to wait for years.
You therefore need access to a High Performance Computing cluster to continue
with reproducing the experiments.
While the density estimation was conducted using python, the main experiments where
conducted in R, mainly relying on the `mlr3` machine learning framework and
`batchtools`. For an introduction on how to conduct large-scale benchmark
experiments with these two packages, you can read the relevant section from the
[mlr3 book](https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html).


**Steps to Reproduce**

1. Clone this GitHub repository:

      ```shell
      git clone git@github.com:slds-lmu/paper_2023_ci_for_ge
      ```

1. Install the renv package and restore the renv environment via:

     ```r
     # from the root of this repository
     renv::restore("path/to/paper_2023_ci_for_ge")
     ```

1. Configure the relevant paths:

    `.Rprofile`:

    Here, you need to adjust - the values of path used to cache the OpenML datasets, and
    - the path where the results are saved. Make sure that the `SAVE_PATH` folder exists.

    `.batchtools.conf` and `slurm_wyoming.tmpl`:

    Here you need to configure how to submit jobs on your HPC cluster.
    You can consult the documentation of [batchtools](https://github.com/mllg/batchtools) on how to do this.

*
1. Generate the data partitions:

   ```shell
   # from the root of this repository
   R experiments/partition.R
   ```

   This will also download and cache the necessary datasets from OpenML.

1. Initialize the experiment design for the resample experiments:

   From the root of this repository run
   ```shell
   R experiments/resample.R
   ```


1. Submit the experiments to the cluster:
   Depending on the configuration of your batchtools config, you might have to customize job submission as well.
   Consult the batchtools documentation on how to do this.

   Executing the file `./experiments/chunk_resample` will chunk the individual resample experiments into batch jobs
   for efficiency. Because of the size of the experiment, it is possible that you have to execute the following commands
   repeatedly. Note that the chunking will only chunk those jobs that were not yet submitted.

   ```R
   # from the root of this repository
   source("experiments/chunk_resample.R")
   submitJobs(chunks)
   ```

   This is by far the most computationally expensive step.
   Note that most of the runtime is due to some expensive resample methods.
   If you are not interested in these methods or this is beyond your computational
   capabilities, removing those from the experiment design in `./experiments/resample.R`
   will considerably reduce the runtime.


1. Evaluate the Results

   After the previous step is finished, run the following commands

   ```shell
   # from the root of this repository
   R experiments/ci.R
   R experiments/truth.R
   R experiments/proxy.R
   ```

1. Merge the result tables:

    TODO

1. Re-create the visualizations from the paper:
