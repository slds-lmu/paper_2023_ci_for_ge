source("renv/activate.R")

# This is where the datasets will be cached

if (dir.exists("/gscratch/sfische6/mlr3oml_cache")) {
  options(mlr3oml.cache = "/gscratch/sfische6/mlr3oml_cache")
} else {
  options(mlr3oml.cache = TRUE)
}

options(mlr3oml.parquet = TRUE)

# This is the path where the experiments results will be saved:
# - RESAMPLE_PATH contains the results of ./experiments/resample.R
#   which are the results of the resample experiments.
# - CI_PATH contains the results of ./experiments/ci.R
#   which are the confidence intervals for the resample experiments.
# - TRUTH_PATH contains the results of ./experiments/truth.R
#   These are the estimated true (expected) risks for the resample experiments.
#   They are used to evaluate the coverage of the confidence intervals.
# - PROXY_PATH contains the results of ./experiments/proxy.R
#   These are the proxy quantities, e.g. for bayle, which are also used
#   to evaluate some of the confidence interval methods.
SAVE_PATH = if (dir.exists("/gscratch/sfische6/benchmarks/ci_for_ge")) {
  "/gscratch/sfische6/benchmarks/ci_for_ge/final"
} else {
  warning("Configure path where experiments will be stored")
}
Sys.setenv(RESAMPLE_PATH = paste0(SAVE_PATH, sep = "_", "resample"))
Sys.setenv(CI_PATH       = paste0(SAVE_PATH, sep = "_", "ci"))
Sys.setenv(PROXY_PATH    = paste0(SAVE_PATH, sep = "_", "proxy"))
Sys.setenv(TRUTH_PATH    = paste0(SAVE_PATH, sep = "_", "truth"))
Sys.setenv(ERROR_PATH    = paste0(SAVE_PATH, sep = "_", "error"))
Sys.setenv(MERGE_PATH    = paste0(SAVE_PATH, sep = "_", "merge"))
