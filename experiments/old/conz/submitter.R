library(batchtools)
library(data.table)
REGISTRY_PATH <- Sys.getenv("RESAMPLE_PATH_CONZ")

reg = loadRegistry(REGISTRY_PATH, writeable = TRUE)

jt = unwrap(getJobTable(findNotStarted()))
cheap_ids = jt[resampling_name == "conservative_z", "job.id"][[1]]
cheap_chunks = data.table(job.id = cheap_ids, chunk = batchtools::chunk(cheap_ids, chunk.size = 40))
submitJobs(cheap_chunks, resources = list(memory = 512 * 8, walltime = 3600 * 12))

expensive_ids = jt[resampling_name != "conservative_z", "job.id"][[1]]
expensive_chunks = data.table(job.id = expensive_ids, chunk = batchtools::chunk(expensive_ids, chunk.size = 10))
submitJobs(expensive_chunks, resources = list(memory = 512 * 16, walltime = 3600 * 12))
