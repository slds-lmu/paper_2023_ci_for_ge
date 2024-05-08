library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("CI_PATH_LM"), writeable = TRUE)

while (TRUE) {
  n = nrow(findQueued()) + nrow(findRunning())
  if (n <= 10000) {
    not_started = findNotSubmitted()[[1]]
    if (nrow(findNotSubmitted()) == 0) break
    not_started = not_started[seq_len(min(length(not_started), 50000))]
    tbl = data.table(job.id = not_started, chunk = batchtools::chunk(not_started, chunk.size = 100))
    submitJobs(tbl, resources = list(memory = 512 * 8))
    print("submitted more, yeah!")
    Sys.sleep(180 * 3)
  } else {
    Sys.sleep(90 * 3)
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
