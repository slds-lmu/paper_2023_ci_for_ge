from pyprojroot.here import here
from be_great import GReaT
import typing as tp
import pandas as pd
from tqdm import tqdm
import torch
import numpy as np
from peft import (
    LoraConfig,
    TaskType,
    get_peft_model
)
import json

def sample(name, n, seed, k, max_length):
    np.random.seed(42)
    torch.manual_seed(42)
    if name == "higgs":
        great = GReaT('distilgpt2')

        # Define LoRA Config
        lora_config = LoraConfig(
            r=16,  # only training 0.16% of the parameters of the model
            lora_alpha=32,
            target_modules=[
                "c_attn"
            ],  # this is specific for gpt2 model, to be adapted
            lora_dropout=0.05,
            bias="none",
            task_type=TaskType.CAUSAL_LM,  # this is specific for gpt2 model, to be adapted
        )
        # add LoRA adaptor
        great.model = get_peft_model(great.model, lora_config)
        great.model.print_trainable_parameters()

        path = str(here("datamodels/models")) + '/' + name
        great.model.load_state_dict(torch.load(path + '/model.pt'))

        # Load attributes
        with open(path + '/config.json', "r") as f:
            attributes = json.load(f)

        # Set all attributes
        for key, v in attributes.items():
            setattr(great, key, v)
            
        model = great
    else:
        path = str(here('datamodels/models/' + name))
        model = GReaT.load_from_dir(path)

    cnt = 0
    
    all_samples = list()
    
    while cnt != n:
        n_samples = min(n - cnt, k)
        new_samples = model.sample(n_samples = n_samples, k = min(n_samples, k), max_length = max_length)
        all_samples.append(new_samples)
        cnt += n_samples
        
    df = pd.concat(all_samples, axis = 0)

    name = str(here('data/simulated')) + '/' + name + '_' + str(n) + '_' + str(seed) + '.parquet'
    df.to_parquet(name, index = False)
    
    return df

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Sample from a GReaT model.')
    parser.add_argument('--model', type=str, help='The name of the model.')
    parser.add_argument('--n', type=int, help='The number of samples to generate.')
    parser.add_argument('--seed', type=int, help='The seed for the random number generator.', default=42)
    parser.add_argument('--k', type=int, help='The seed for the random number generator.', default=512)
    parser.add_argument('--max_length', type=int, help='Maximum number of tokens to generate', default=200)
    args = parser.parse_args()
    
    samples = sample(args.model, args.n, args.seed, args.k, args.max_length)
    
    None
    
    
