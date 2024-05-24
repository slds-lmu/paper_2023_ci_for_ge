library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_RIDGE"), writeable = TRUE)

jt = unwrap(getJobTable())

cheap_resamplings = (jt[size == 1000, ]$resampling_name) |> unique()
expensive_resamplings = setdiff(unique(jt$resampling_name), cheap_resamplings)

jt = jt[map_lgl(resampling_name, function(name) name %in% expensive_resamplings), ]
expensive_ids = jt$job.id

#super_expensive_resamplings = c("austern_zhou_rep", "bootstrap_ccv", "nested_cv")
#expensive_resamplings = setdiff(expensive_resamplings, super_expensive_resamplings)

while (TRUE) {
  n = nrow(findQueued()) + nrow(findRunning())
  print(getStatus())
  if (n <= 30000) {
    not_started = findNotStarted()[[1]]
    not_started = not_started[not_started %in% expensive_ids]
    if (nrow(findNotStarted()) == 0) break
    not_started = not_started[seq_len(min(length(not_started), 50000))]
    tbl = data.table(job.id = not_started, chunk = batchtools::chunk(not_started, chunk.size = 200))
    submitJobs(tbl, resources = list(memory = 512 * 32L))
    print("submitted more, yeah!")
    Sys.sleep(1800)
  } else {
    Sys.sleep(900)
  }
}       
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
