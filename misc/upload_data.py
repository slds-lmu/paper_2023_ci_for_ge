import openml
import arrow
from pyprojroot.here import here
import pandas as pd

from openml.datasets.functions import create_dataset

# Will change that later.

desc = "See [https://github.com/slds-lmu/paper_2023_ci_for_ge](https://github.com/slds-lmu/paper_2023_regression_suite) for a description."

# First the artificial ones

datasets = {
    "bates_classif_100" : {"target" : "y"},
    "bates_classif_20" : {"target" : "y"},
    "bates_regr_100" : {"target" : "y"},
    "bates_regr_20" : {"target" : "y"},
    "breast" : {"target" : "y"},
    "chen_10_null" : {"target" : "y"},
    "chen_10" : {"target" : "y"},
    "colon" : {"target" : "y"},
    "friedman1" : {"target" : "y"},
    "prostate" : {"target" : "y"},
}

for name, info in datasets.items():
    df = pd.read_parquet(str(here(f"data/artificial/{name}.pq")))

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
        collection_date='2023',
        language='8/8/2023',
        citation = "See https://github.com/slds-lmu/paper_2023_regression_suite"


    )

    new_dataset.publish()





