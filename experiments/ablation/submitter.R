library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_ABLATION"), writeable = TRUE)

jt = unwrap(getJobTable(findExpired()))

#expensive_ids = jt[resampling_id == "conservative_z", "job.id"][[1L]]

#chunks = data.table(job.id = expensive_ids, chunk = batchtools::chunk(expensive_ids, chunk.size = 10))


other_ids = jt[resampling_id != "conservative_z" & size <= 500, "job.id"][[1L]]

chunks = data.table(job.id = other_ids, chunk = batchtools::chunk(other_ids, chunk.size = 200))

#submitJobs(chunks, resource = list(memory = 512L * 8L))

