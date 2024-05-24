# This file is used to submit the experiments defined in ./experiments/design.R
library(batchtools)
library(mlr3)

REGISTRY_PATH = Sys.getenv("RESAMPLE_PATH")

reg = loadRegistry(REGISTRY_PATH, writeable = TRUE)

ids = sample(setdiff(findNotStarted()[[1]], findQueued()[[1]]), 200000L)
chunks = data.table(
  job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 100, shuffle = FALSE)
)

#submitJobs(chunks)
