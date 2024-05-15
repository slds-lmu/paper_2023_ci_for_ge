from pyprojroot.here import here
from be_great import GReaT
import typing as tp
import pandas as pd
from tqdm import tqdm
import torch
import numpy as np
import json

def sample(name, n, seed, k, max_length):
    np.random.seed(42)
    torch.manual_seed(42)
    path = str(here('data/models/' + name))
    model = GReaT.load_from_dir(path)

    cnt = 0

    all_samples = list()

    while cnt != n:
        n_samples = min(n - cnt, k)
        new_samples = model.sample(n_samples = n_samples, k = min(n_samples, k), max_length = max_length)
        all_samples.append(new_samples)
        cnt += n_samples

    df = pd.concat(all_samples, axis = 0)

    name = str(here('data/density-estimate')) + '/' + name + '_' + str(n) + '_' + str(seed) + '.parquet'
    df.to_parquet(name, index = False)

    return df

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Sample from a GReaT model.')
    parser.add_argument('--model', type=str, help='The name of the model.')
    parser.add_argument('--n', type=int, help='The number of samples to generate.')
    parser.add_argument('--seed', type=int, help='The seed for the random number generator.', default=42)
    parser.add_argument('--k', type=int, help='The maximum number of samples to generate at once.', default=512)
    parser.add_argument('--max_length', type=int, help='Maximum number of tokens to generate', default=200)
    args = parser.parse_args()

    samples = sample(args.model, args.n, args.seed, args.k, args.max_length)
