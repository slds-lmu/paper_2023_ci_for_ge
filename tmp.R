import pandas as pd

original = pd.read_csv('data/original/adult.csv')
simulated = pd.read_parquet('data/simulated/adult_50000_42.pq')

choices = original['native-country'].unique()
values = simulated['native-country'].unique()


def replace_value(val, choices):
    """Replace a value based on 'starts with' condition."""
    matches = [choice for choice in choices if choice.startswith(val)]
    
    # Return the matching value if only one match is found, otherwise None
    return matches[0] if len(matches) == 1 else None


for val in values: s

replace_values(values, choices)