# Sometimes, the simulation is not perfect (e.g. wrong factor levels are sampled).
# Here we do some postprocessing to fix this.
import pandas as pd
import re
from pyprojroot.here import here
import numpy as np

def get_original(s):
    match = re.match(r'^(.+?)_\d+', s)
    if match:
        return match.group(1)
    else:
        return None

def get_unique_categorical_values(df):
    categorical_columns = df.select_dtypes(include=['object', 'category']).columns
    unique_values = {col: df[col].unique().tolist() for col in categorical_columns}
    return unique_values


def replace_value(val, choices):
    """Replace a value based on 'starts with' condition."""
    matches = [choice for choice in choices if choice.startswith(val)]
    
    # Return the matching value if only one match is found, otherwise None
    return matches[0] if len(matches) == 1 else None

# sometimes the LLM says e.g. "United-" instead of "United-States"
# We check whether there is any available category that starts with "United-"
# if there is ONE match we pick it, otherwise we set the value to None
def fix_categories(df, allowed_values):
    for colname, allowed_values in allowed_values.items():
        df[colname] = df[colname].apply(lambda x: replace_value(x, allowed_values))
        
    return df

def main(simulated_name):
    original_name = get_original(simulated_name)
    original = pd.read_csv(str(here('data/original/' + original_name + '.csv')))
    simulated = pd.read_parquet(str(here('data/simulated/' + simulated_name + '.parquet')))
    unique_values = get_unique_categorical_values(original)
    
    print('Number of rows before: ' + str(simulated.shape[0]))
    processed = fix_categories(simulated, unique_values)
    processed.dropna(inplace = True)
    
    print(processed.isna().sum())

    if original_name == 'covertype':
        # encode Class column as categorical
        processed['Y'] = processed['Y'].astype(int).astype(str).astype('category')


    # subset dataframe to first 5 100 000 rows
    processed = processed.iloc[:5100000, :]    

    print(processed.dtypes)

    processed.to_parquet(str(here('data/simulated/' + 'simulated_' + original_name + '.parquet')), index = False)
    
    print('Number of rows afterwards: ' + str(processed.shape[0]))

    None



if __name__ == "__main__":

    dataset_names = [
        "adult_6000000_42",
        "covertype_6000000_42",
        "diamonds_6000000_42",
        "electricity_6000000_42",
        "physiochemical_protein_6000000_42",
        "sgemm_gpu_kernel_performance_6000000_42",
        "video_transcoding_6000000_42"
    ]


    for dataset_name in dataset_names: 
        main(dataset_name)
