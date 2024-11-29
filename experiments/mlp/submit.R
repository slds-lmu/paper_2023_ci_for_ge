library(batchtools)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_MLP"), writeable = TRUE)

jt = unwrap(getJobTable())

jtsub = jt[size == 5000 & repl <= 50 & resampling_id != "insample" & task_name == "higgs" ,]

# submit 50 reps of higgs dataset with holdout 

submitJobs(jtsub$job.id, resources = list(walltime = 3600 * 24, ncpus = 48, memory = 1024 * 8, partition = "mb"))

jtsub = jt[size == 5000 & repl <= 50 & resampling_id == "insample" & task_name == "higgs" ,]
submitJobs(1, resources = list(walltime = 3600 * 4, ncpus = 25, memory = 1024 * 4, partition = "mb-a30", gres = "gpu:1"))
#submitJobs(2, resources = list(walltime = 3600 * 2, ncpus = 1, memory = 1024 * 8, partition = "mb-h100", gres = "gpu:1"))

