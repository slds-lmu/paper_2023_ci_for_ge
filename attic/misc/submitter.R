library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("CI_PATH"), writeable = TRUE)

while (TRUE) {
  n = nrow(findQueued()) + nrow(findRunning())
  if (n <= 10000) {
    if (nrow(findNotStarted()) == 0) break
    not_started = findNotStarted()[[1]]
    not_started = not_started[seq_len(min(length(not_started), 100000))]
    tbl = data.table(job.id = not_started, chunk = batchtools::chunk(not_started, chunk.size = 200))
    submitJobs(tbl)
    print("submitted more, yeah!")
    Sys.sleep(1800)
  } else {
    Sys.sleep(900)
  }

}       
