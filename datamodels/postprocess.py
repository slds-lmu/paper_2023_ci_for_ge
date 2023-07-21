# Sometimes, the simulation is not perfect (e.g. wrong factor levels are sampled).
# Here we do some postprocessing to fix this.
import pandas as pd
import re
from pyprojroot.here import here

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

def remove_unseen_categories(df, allowed_values):
    for colname, allowed_values in allowed_values.items():
        df = df[df[colname].isin(allowed_values)]
        
    return df
    


def main(simulated_name):
    original_name = get_original(simulated_name)
    original = pd.read_csv(str(here('data/original/' + original_name + '.csv')))
    simulated = pd.read_parquet(str(here('data/simulated/' + simulated_name + '.pq')))
    unique_values = get_unique_categorical_values(original)
    
    processed = remove_unseen_categories(simulated, unique_values)
    
    print(processed)
    print(original.shape)
    print(processed.shape)


    None



if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Postprocess the simulated data.')
    parser.add_argument('--name', type=str, help='The name of the dataset.')
    args = parser.parse_args()
    # Call the main function with the parsed arguments
    main(args.name)