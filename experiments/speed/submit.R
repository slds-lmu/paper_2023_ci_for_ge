library(batchtools)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_COMPLEX"), writeable = TRUE)

jt = unwrap(getJobTable())

jt = jt[repl <= 100, ]

chunks = batchtools::chunk(jt$job.id, chunk.size = 100)

tbl = data.table(job.id = jt$job.id, chunk = chunks) 

submitJobs(tbl, resources = list(walltime = 3600 * 6, memory = 512 * 8L, partition = "mb"))

