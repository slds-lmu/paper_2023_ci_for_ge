from be_great import GReaT
from pyprojroot.here import here
import pickle
import numpy as np
import os
import torch
import pandas as pd
import json


def main(id, batch_size = 32, epochs = 200, save_steps = 5000, logging_steps = 5000, efficient_finetuning = ""): 

    # set numpy seed
    np.random.seed(42)
    torch.manual_seed(42)
    name = str(id)

    data = pd.read_csv(here('data/original/' + str(id) + '.csv'), index_col=False)
    
    with open(here('data/original/' + str(id) + '.json'), 'rb') as file:
        info = json.load(file)
        
    train_ids = info['train_ids']
    # test_ids = info['test_ids']
    
    n = len(data)
    
    data_train = data.iloc[train_ids]

    if not os.path.exists(here('datamodels/models')): 
        os.makedirs(here('datamodels/models'))
    
    experiment_dir = str(here('datamodels/models/' + name))

    model = GReaT(
        llm='distilgpt2', 
        batch_size=batch_size,
        epochs=epochs,
        save_steps = save_steps,
        experiment_dir = experiment_dir,
        logging_steps = logging_steps,
        efficient_finetuning = efficient_finetuning
    )

    trainer = model.fit(data_train, resume_from_checkpoint = os.path.exists(experiment_dir))
    model.save(experiment_dir)

    trainer.save_state()

    return trainer


if __name__ == '__main__': 
    # Parse the command-line arguments
    import argparse
    parser = argparse.ArgumentParser(description='Train a GReaT model.')
    parser.add_argument('--id', type=str, help='The data id.')
    parser.add_argument('--batch_size', default=32, type=int, help='The batch size for training.')
    parser.add_argument('--epochs', type=int, help='Number of training epochs.')
    parser.add_argument('--save_steps', type=int, help='After how many steps the model is saved.')
    parser.add_argument('--logging_steps', type=int, help='How often the logs are saved.')
    parser.add_argument('--efficient_finetuning', type=str, help='Whether to use e.g. lora.')
    args = parser.parse_args()
    
    if args.logging_steps is None: 
        args.logging_steps = args.save_steps

    # Call the main function with the parsed arguments
    main(args.id, args.batch_size, args.epochs, args.save_steps, args.logging_steps, args.efficient_finetuning)