library(batchtools)

reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_MLP"), writeable = TRUE, conf.file = here::here("experiments/mlp/batchtools.conf.R"))

jt = unwrap(getJobTable())
