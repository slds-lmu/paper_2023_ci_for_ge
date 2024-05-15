import pandas as pd
from pyprojroot import here

dataset_names = [
    "simulated_adult",
    "simulated_bank_marketing",
    "simulated_covertype",
    "simulated_diamonds",
    "simulated_electricity",
    "simulated_physiochemical_protein",
    "simulated_sgemm_gpu_kernel_performance",
    "simulated_video_transcoding"
]

for dataset_name in dataset_names:
    print(dataset_name)
    ds = pd.read_parquet(str(here('data/density-estimate/' + dataset_name + '.parquet')))

    print(ds.isna().sum())
    print(ds.dtypes)
