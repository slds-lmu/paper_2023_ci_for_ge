library(batchtools)
library(data.table)
reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_MORE"), writeable = TRUE)


while (TRUE) {
	print(getStatus())
	if (nrow(findRunning()) > 20000) {
		Sys.sleep(1000)
		next
	}
	ns = findNotStarted()[[1]]
	if (length(ns) == 0) break
	ns = ns[seq_len(min(length(ns), 50000))]
	chunks = data.table(job.id = ns, chunk = batchtools::chunk(ns, chunk.size = 50)) 
	submitJobs(chunks, resources = list(walltime = 3600 * 10, memory = 512 * 8)) 
	Sys.sleep(1000)
			c
}

