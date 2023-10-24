import pandas as pd

original = pd.read_csv('data/original/diamonds.csv')
simulated = pd.read_parquet('data/simulated/diamonds_6000000_42.parquet')
processed = pd.read_parquet('data/simulated/diamonds_6000000_42_processed.parquet')

choices = original['native-country'].unique().tolist()
values = simulated['native-country'].unique().tolist()


def replace_value(val, choices):
    """Replace a value based on 'starts with' condition."""
    matches = [choice for choice in choices if choice.startswith(val)]
    
    # Return the matching value if only one match is found, otherwise None
    return matches[0] if len(matches) == 1 else None


for val in values: 
    print(val + ' = ' + replace_value(val, choices))