cluster.functions = batchtools::makeClusterFunctionsSlurm("slurm_wyoming.tmpl", array.jobs = TRUE)
default.resources = list(walltime = 36000L, memory = 512L * 32, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton")

max.concurrent.jobs = 9999L
