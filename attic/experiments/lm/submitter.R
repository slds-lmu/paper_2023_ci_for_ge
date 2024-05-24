library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_LM"), writeable = TRUE)

jt = unwrap(getJobTable())

cheap_resamplings = (jt[size == 1000, ]$resampling_name) |> unique()

expensive_resamplings = setdiff(unique(jt$resampling_name), cheap_resamplings)
super_expensive_resamplings = c("austern_zhou_rep", "bootstrap_ccv", "nested_cv")
expensive_resamplings = setdiff(expensive_resamplings, super_expensive_resamplings)

ids = jt[resampling_name %in% expensive_resamplings, "job.id"][[1L]]
done = findDone()[[1]]
ids = ids[ids %nin% done]
chunks = data.table(job.id = ids, chunk = batchtools::chunk(ids, chunk.size = 100))

submitJobs(chunks, resources = list(memory = 512 * 32L))

# first finalize experiments for n = 500, then the rest

#while (TRUE) {
#  n = nrow(findQueued()) + nrow(findRunning())
#  if (n <= 10000) {
#    not_started = findNotStarted()[[1]]
#    not_started = not_started[not_started %in% ids_500]
#    if (nrow(findNotStarted()) == 0) break
#    not_started = not_started[seq_len(min(length(not_started), 50000))]
#    tbl = data.table(job.id = not_started, chunk = batchtools::chunk(not_started, chunk.size = 200))
#    submitJobs(tbl)
#    print("submitted more, yeah!")
#    Sys.sleep(1800)
#  } else {
#    Sys.sleep(900)
#  }
#}       
#
#while (TRUE) {
#  n = nrow(findQueued()) + nrow(findRunning())
#  if (n <= 10000) {
#    if (nrow(findNotStarted()) == 0) break
#    not_started = findNotStarted()[[1]]
#    not_started = not_started[seq_len(min(length(not_started), 50000))]
#    tbl = data.table(job.id = not_started, chunk = batchtools::chunk(not_started, chunk.size = 200))
#    submitJobs(tbl)
#    print("submitted more, yeah!")
#    Sys.sleep(1800)
#  } else {
#    Sys.sleep(900)
#  }
#}       
