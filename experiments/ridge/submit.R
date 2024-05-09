library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_LM"), writeable = TRUE)

jt = unwrap(getJobTable())
jt500 = jt[size == 500L,]

chunks_500 = data.table(job.id = jt500$job.id, chunk = batchtools::chunk(jt500$job.id, chunk.size = 200L))
