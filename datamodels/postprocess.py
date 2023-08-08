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


def replace_value(val, choices):
    """Replace a value based on 'starts with' condition."""
    matches = [choice for choice in choices if choice.startswith(val)]
    
    # Return the matching value if only one match is found, otherwise None
    return matches[0] if len(matches) == 1 else None

def fix_categories(df, allowed_values):
    for colname, allowed_values in allowed_values.items():
        df[colname] = df[colname].apply(lambda x: replace_value(x, allowed_values))

        # Calculate frequency
        value_counts = df[colname].value_counts(normalize=True)
        rare_values = value_counts[value_counts < 0.02].index

        # Replace rare values with "rare_values"
        unique_vals = df[colname].unique().tolist()
        def make_unique(l, c):
            original_c = c
            counter = 1
            while c in l:
                c = original_c + "_" + str(counter)
                counter += 1
            return c

        new_name = make_unique(unique_vals, 'rare_values')
        df[colname] = df[colname].replace(rare_values, 'rare_values')

        
    return df
    


def main(simulated_name):
    original_name = get_original(simulated_name)
    original = pd.read_csv(str(here('data/original/' + original_name + '.csv')))
    simulated = pd.read_parquet(str(here('data/simulated/' + simulated_name + '.pq')))
    unique_values = get_unique_categorical_values(original)
    
    print('Number of rows before: ' + str(simulated.shape[0]))
    processed = fix_categories(simulated, unique_values)

    name = simulated_name + '_processed.pq'

    processed.dropna()

    processed.to_parquet(str(here('data/simulated/' + name)), index = False)
    
    print('Number of rows afterwards: ' + str(processed.shape[0]))

    None



if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Postprocess the simulated data.')
    parser.add_argument('--name', type=str, help='The name of the dataset.')
    args = parser.parse_args()
    # Call the main function with the parsed arguments
    main(args.name)


