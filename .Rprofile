source("renv/activate.R")

options(mlr3oml.cache = "/gscratch/sfische6/mlr3oml_cache")

SAVE_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/test1234"

if (!dir.exists(SAVE_PATH)) dir.create(SAVE_PATH)

Sys.setenv(RESAMPLE_PATH = paste0(SAVE_PATH, sep = "_", "resample"))
Sys.setenv(CI_PATH       = paste0(SAVE_PATH, sep = "_", "ci"))
Sys.setenv(PROXY_PATH    = paste0(SAVE_PATH, sep = "_", "proxy"))
Sys.setenv(TRUTH_PATH    = paste0(SAVE_PATH, sep = "_", "truth"))
