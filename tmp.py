#import pandas as pd
#import os
#
#
#n = {
#    "diamonds" : 53940,
#    "ph"
#
#}
#
#
#
## conver the below into a python dictionary
#
#* diamonds: n = 53940
#* physiochemical_protein: n = 45730
#* vide_transcoding: n = 68784
#* sgemm_gpu_kernel_performance: n = 241600,
#* adult: n = 48842
#* bank_marketing: n = 45211
#* electricity: n = 45312
#* covertype: n = 566602
#* higgs: n = 11000000


import json
import typing as tp
import logging

import numpy as np
import pandas as pd

from tqdm import tqdm

import torch
from transformers import AutoTokenizer, AutoModelForCausalLM, TrainingArguments

from be_great.great_dataset import GReaTDataset, GReaTDataCollator
from be_great.great_start import (
    GReaTStart,
    CategoricalStart,
    ContinuousStart,
    RandomStart,
    _pad_tokens,
)
from be_great.great_trainer import GReaTTrainer
from be_great.great_utils import (
    _array_to_dataframe,
    _get_column_distribution,
    _convert_tokens_to_text,
    _convert_text_to_tabular_data,
    _partial_df_to_promts,
    bcolors,
)

from peft import (
    LoraConfig,
    get_peft_model,
    prepare_model_for_int8_training,
    TaskType,
)

from be_great import GReaT

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

great.model.load_state_dict(torch.load("model.pt"))

import json
# Load attributes
with open("config.json", "r") as f:
    attributes = json.load(f)

# Set all attributes
for k, v in attributes.items():
    setattr(great, k, v)
    
def helper():
    None