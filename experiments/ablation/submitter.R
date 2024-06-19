library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_ABLATION"), writeable = TRUE)

jt = unwrap(getJobTable())

expensive_ids = jt[resampling_id == "conservative_z", "job.id"][[1L]]

chunks = data.table(job.id = expensive_ids, chunk = batchtools::chunk(expensive_ids, chunk.size = 10))

