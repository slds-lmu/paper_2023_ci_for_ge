Overview of the folders:

**raw**

This folder contains all raw data that is not processed by any files in this repository.
These files need to be downloaded from the zenodo link given in the paper.

**ablation**

This folder contains the results of the parameter investigation of the well-performing inference methods:
For each method an aggregated and unaggregated file exists.

To recreate this, run `make process_ablation` from the root of this repository.
For this you first need to download the raw data from zenodo and move it into `./results/raw`

**main**

This folder contains the main result table in (un)aggregated form.

To recreate those files, first download the files into the `raw` folder and then run `make process`
For this you first need to download the raw data from zenodo and move it into `./results/raw`
