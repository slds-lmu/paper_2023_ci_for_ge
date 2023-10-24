# This file is used to submit the experiments defined in ./experiments/design.R
library(batchtools)
library(mlr3)

TEST = TRUE

if (TEST) {
  # BEWARE THE CORRECT NUMBER AT THE END
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/newtest2"
} else {
  REGISTRY_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/final"
}

reg = loadRegistry(REGISTRY_PATH, writeable = TRUE)

job_table = getJobTable(findExpired())

ids = job_table$job.id
chunks = data.table(
  job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 10, shuffle = FALSE)
)

submitJobs(chunks)

reg = batchtools::loadRegistry(REGISTRY_PATH)