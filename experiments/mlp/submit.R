library(batchtools)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_MLP"), writeable = TRUE, conf.file = here::here("experiments/mlp/batchtools.conf.R"))

#jt = unwrap(getJobTable(findNotDone()))
# for insample we need different paralleliation	     
#jtsub = jt[repl <= 100 & repl > 50 & resampling_id == "insample" & size == 5000, ]
#submitJobs(jtsub$job.id)
