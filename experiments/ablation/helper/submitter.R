library(batchtools)
library(data.table)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_ABLATION"), writeable = TRUE)

while (TRUE) {
  print(getStatus())
  n = nrow(findQueued()) + nrow(findRunning())
  if (n <= 20000) {
    not_started = findExpired()[[1]]
    if (length(not_started) == 0) {
	break
    }
    not_started = not_started[seq_len(min(length(not_started), 20000))]
    tbl = data.table(job.id = not_started, chunk = batchtools::chunk(not_started, chunk.size = 100))
    try(submitJobs(tbl, resources = list(memory = 512 * 16, walltime = 3600 * 3)), silent = TRUE)
    print("submitted more, yeah!")
    Sys.sleep(600)
  } else {
    Sys.sleep(300)
  }
}
