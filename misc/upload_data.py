import openml
import arrow
from pyprojroot.here import here
import pandas as pd
import json

from openml.datasets.functions import create_dataset

# Will change that later.

desc = "See [https://github.com/slds-lmu/paper_2023_ci_for_ge](https://github.com/slds-lmu/paper_2023_ci_for_ge) for a description."

id_dict = dict()

# First the artificial ones

datasets = {
    #"bates_classif_20" : {"target" : "y"},
    #"bates_regr_20" : {"target" : "y"}, Done
    #"colon" : {"target" : "y"},
    #"friedman1" : {"target" : "y"},
    # TODO
    "bates_regr_100" : {"target" : "y"},
    #"bates_classif_100" : {"target" : "y"},
    #"breast" : {"target" : "y"},
    #"chen_10_null" : {"target" : "y"},
    #"chen_10" : {"target" : "y"},
    #"prostate" : {"target" : "y"},
}

for name, info in datasets.items():
    df = pd.read_parquet(str(here(f"data/artificial/{name}.pq")))

    object_cols = df.select_dtypes(include=['object']).columns
    df[object_cols] = df[object_cols].astype('category')

    print('uploading: ' + name)
    new_dataset = create_dataset(
        # The name of the dataset (needs to be unique).
        # Must not be longer than 128 characters and only contain
        # a-z, A-Z, 0-9 and the following special characters: _\-\.(),
        name=name,
        # Textual description of the dataset.
        description=desc,
        # The person who created the dataset.
        creator="Sebastian Fischer",
        # People who contributed to the current version of the dataset.
        contributor=None,
        licence="CC BY 4.0",
        # Name of the target. Can also have multiple values (comma-separated).
        default_target_attribute=info["target"],
        # The attribute that represents the row-id column, if present in the
        # dataset.
        row_id_attribute=None,
        # Attribute or list of attributes that should be excluded in modelling, such as
        # identifiers and indexes. E.g. "feat1" or ["feat1","feat2"]
        ignore_attribute=None,
        data=df,
        attributes="auto",
        collection_date='8/8/2023',
        language='English',
        citation = "See https://github.com/slds-lmu/paper_2023_ci_for_ge"
    )

    new_dataset.publish()

    print('done')



simulated_datasets = [
    #"adult",
    #"bank_marketing",
    #"covertype",
    #"diamonds",
    #"electricity",
    #"physiochemical_protein",
    #"sgemm_gpu_kernel_performance",
    #"video_transcoding",
]

for name in simulated_datasets:
    # read as json 
    with open(here(f"data/original/{name}.json"), 'r') as file:
        info = json.load(file)

    simulated_name = 'simulated_' + name
    df = pd.read_parquet(str(here(f"data/simulated/{simulated_name}.pq")))

    object_cols = df.select_dtypes(include=['object']).columns
    df[object_cols] = df[object_cols].astype('category')

    print('uploading: ' + name)
    new_dataset = create_dataset(

        # The name of the dataset (needs to be unique).
        # Must not be longer than 128 characters and only contain
        # a-z, A-Z, 0-9 and the following special characters: _\-\.(),
        name=simulated_name,
        # Textual description of the dataset.
        description=desc,
        # The person who created the dataset.
        creator="Sebastian Fischer",
        # People who contributed to the current version of the dataset.
        contributor=None,
        licence="CC BY 4.0",
        # Name of the target. Can also have multiple values (comma-separated).
        default_target_attribute=info["target_name"],
        # The attribute that represents the row-id column, if present in the
        # dataset.
        row_id_attribute=None,
        # Attribute or list of attributes that should be excluded in modelling, such as
        # identifiers and indexes. E.g. "feat1" or ["feat1","feat2"]
        ignore_attribute=None,
        data=df,
        attributes="auto",
        collection_date='8/8/2023',
        language='English',
        citation = "See https://github.com/slds-lmu/paper_2023_ci_for_ge"
    )

    identifier = new_dataset.publish()
    id_dict[name] = identifier

    print('done')



# now higgs

if False: 
    print("uploading higgs:")
    df = pd.read_parquet(str(here(f"data/subset/subset_higgs.pq")))
    object_cols = df.select_dtypes(include=['object']).columns
    df[object_cols] = df[object_cols].astype('category')

    higgs_dataset = create_dataset(
        # The name of the dataset (needs to be unique).
        # Must not be longer than 128 characters and only contain
        # a-z, A-Z, 0-9 and the following special characters: _\-\.(),
        name="subset_higgs",
        # Textual description of the dataset.
        description=desc,
        # The person who created the dataset.
        creator="Daniel Whiteson, University of California Irvine",
        # People who contributed to the current version of the dataset.
        contributor="Sebastian Fischer",
        licence="CC BY 4.0", # from UCI
        # Name of the target. Can also have multiple values (comma-separated).
        default_target_attribute="Target",
        # The attribute that represents the row-id column, if present in the
        # dataset.
        row_id_attribute=None,
        # Attribute or list of attributes that should be excluded in modelling, such as
        # identifiers and indexes. E.g. "feat1" or ["feat1","feat2"]
        ignore_attribute=None,
        data=df,
        attributes="auto",
        collection_date='8/8/2023',
        language='English',
        citation = "Whiteson,Daniel. (2014). HIGGS. UCI Machine Learning Repository. https://doi.org/10.24432/C5V312."
    )

    identifier = higgs_dataset.publish()
    id_dict[name] = identifier
    print("done")



id_df = pd.DataFrame(list(id_dict.items()), columns=['dataset', 'id'])

id_df.to_csv(here("data/uploaded_ids.csv"), index=False)