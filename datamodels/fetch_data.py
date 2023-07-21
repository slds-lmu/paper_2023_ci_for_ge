# download OpenML task with id 
from pyprojroot.here import here
from sklearn.datasets import fetch_openml
import os
import pickle
import pandas as pd
from sklearn.model_selection import train_test_split
import json

datasets = {
    # regression
    'diamonds': 44979,
    'physiochemical_protein' : 44963,
    'video_transcoding' : 44974, 
    'sgemm_gpu_kernel_performance' : 44961,

    # classification
    'adult' : 1590,
    'bank_marketing' : 1461,
    'electricity' : 151,
    'covertype' : 44121,
    'higgs' : 45570,

    # for testing
    # 'sarcos' : 44976,
    'iris' : 61,
}

def main():
    for name, id in datasets.items():
        data = fetch_openml(data_id = id, parser = 'auto')
        
        features = data.data
        target = data.target
        df = pd.concat([features, target], axis = 1)
        
        df[df.select_dtypes(['category']).columns] = df.select_dtypes(['category']).apply(lambda x: x.astype('object'))
        
        # only the adult dataset has missing values (3620) which are dropped
        n1 = len(df)
        df.dropna(inplace=True)
        df.reset_index(drop=True, inplace=True)
        n2 = len(df)
        
        data_train, data_test = train_test_split(df, test_size=0.2, random_state=42)

        train_ids = data_train.index
        test_ids = data_test.index
        
        object = {
            'name' : name,
            'target_name' : data.target_names, 
            'train_ids' : train_ids.to_list(),
            'test_ids' : test_ids.to_list(),
            'n_missing' : n1 - n2,
        }
        
        # save the data as a pkl in the data folder of the parent directory
        if not os.path.exists(here('data/original')):
            os.makedirs(here('data/original'))

        with open(here('data/original/' + object['name'] + '.json'), 'w') as file:
            json.dump(object, file)

        df.to_csv(here('data/original/' + object['name'] + '.csv'), index=False)
        df.to_parquet(here('data/original/' + object['name'] + '.pq'), index=False)

        # create a pandas dataframe with columns 'name', 'target_name', 'n2', 'n_missing'

        table = pd.DataFrame(columns=['name', 'target_name', 'n', 'n_missing'])

        # fill it with the informatio


if __name__ == '__main__':
    main()
