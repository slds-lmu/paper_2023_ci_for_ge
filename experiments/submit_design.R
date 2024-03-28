# This file is used to submit the experiments defined in ./experiments/design.R
library(batchtools)
library(mlr3)

REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/run_big11"

reg = loadRegistry(REGISTRY_PATH, writeable = TRUE)

job_table = getJobTable()

ids = job_table$job.id
chunks = data.table(
  job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 10, shuffle = FALSE)
)

#submitJobs(chunks)
