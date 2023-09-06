# This file is used to submit the experiments defined in ./experiments/design.R
library(batchtools)
library(mlr3)

if (TEST) {
  # BEWARE THE CORRECT NUMBER AT THE END
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/test7"
} else {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

job_table = getJobTable(findExpired())

ids = job_table$job.id
chunks = data.table(
  job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 50, shuffle = FALSE)
)

submitJobs(chunks)

reg = batchtools::loadRegistry(REGISTRY_PATH)