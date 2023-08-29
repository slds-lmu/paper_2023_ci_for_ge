# This file is used to submit the experiments defined in ./experiments/design.R

library(batchtools)
library(mlr3)

if (TEST) {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/test"
} else {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

reg = batchtools::loadRegistry(REGISTRY_PATH)