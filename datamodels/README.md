# Datamodels

The code in this folder was used to generate the datasets used to compare the confidence interval methods.

## Simplistically Generated Data

The folder `./artificial/` contains the code that is used to simulate the datasets that do not use the LLM estimation
approach. This includes the *bates*, *friedman1* and *chen* methods, as well as the approach where
covariance matrices estimated from real-world data are used to sample from multivariate normal distributions.
The data to estimate these covariance matrices can be found in `./data/original/covariance`.

To reproduce the datasets, just run the following commands from the root of this repository.

```r
source("./datamodels/artificial/bates.R")
source("./datamodels/artificial/chen.R")
source("./datamodels/artificial/friedman1.R")

# estimate the covariance matrices
source("./datamodels/artificial/process_data_cov.R")
# use covariance matrices to simulate data
source("./datamodels/artificial/janitza.R")
```

The results will be stored in `./data/simulated`.

## Generate Realistically Simulated Data

The other datasets that were generated for the experiments first estimated densities from real-world datasets using the
[GReaT](https://github.com/kathrinse/be_great) approach.
The steps to reproduce this are:

1. Restore the conda environment stored in `environment.yml` by running:

   ```bash
   conda env create -f environment.yml
   ```
1. Download the datasets for which the densities are estimated and remove missing values by running `fetch_data.py`.
   This also creates train test splits, where the training data is used to estimate the densities and the test data
   is later used to evaluate the qualtity of the generated data:

   ```bash
   python fetch_data.py
   ```

   The results of this step are then stores in `./data/original/`.

1. Estimate the densities from the real-world datasets by fine-tuning GPT-2:
   For all datasets, a batch size of 32 was unused.
   For the *covertype* data, 20 epochs were used and for the *sgemm_gpu_kernel* data 40 epochs were used as these
   datasets were much larger than the other datasets. 200 epochs were used for all other datasets.

   To e.g. train the model for *covertype*, run

   ```bash
   python train.py --id covertype --epochs 20
   ```

1. Use the estimated densities to sample datasets.
   For all seven datasets, 6 000 000 observations were sampled with a seed of 42.
   The results of this are stored in `./data/simulated/`.

   ```bash
   python sample.py --model covertype --n 6000000
   ```

1. Postprocess the simulated datasets:
   This ensures that the simulated data has no new categorical levels and correctly
   encodes the covertype target as a categorical.
   The resulting datasets are stored as parquet files in `./data/simulated/`.

   ```bash
   python postprocess.py
   ```

## Evaluate the Realistically Simulated Data

The entrypoint for the evaluation of the realistically simulated data are the datasets uploaded to OpenML.
It is therefore possible to reproduce this step without estimating the densities and sampling from them.
You do, however, need to run `fetch_data.py` (as described above) to have access to the original datasets.


1. If you haven't, restore the reproducible R environment defined in `./renv.lock`:

   For that, run the following commands from the root directory of this repository:

   ```R
   # install renv if you don't have it installed yet
   install.packages("renv")
   renv::restore()
   ```

1. Evaluate all datasets:

   Run the following command from the root of this repository:

   ```bash
   Rscript ./datamodels/evaluate/run.R
   ```

   The results of this are stored in `./datamodels/evaluation/results`
