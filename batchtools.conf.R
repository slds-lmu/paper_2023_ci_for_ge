cluster.functions = batchtools::makeClusterFunctionsSlurm("slurm_wyoming.tmpl", array.jobs = FALSE)
default.resources = list(walltime = 3600L * 5, memory = 512L * 64, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton-knl")

max.concurrent.jobs = 9999L
