cluster.functions = batchtools::makeClusterFunctionsSlurm("slurm_wyoming.tmpl", array.jobs = TRUE)
default.resources = list(walltime = 3600L * 10L, memory = 512L * 8L, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton")

max.concurrent.jobs = 9999L
