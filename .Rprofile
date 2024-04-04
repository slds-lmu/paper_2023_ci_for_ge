source("renv/activate.R")

# This is where the datasets will be cached
options(mlr3oml.cache = "/gscratch/sfische6/mlr3oml_cache")


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
SAVE_PATH = "/gscratch/sfische6/benchmarks/ci_for_ge/test1234"
if (!dir.exists(SAVE_PATH)) dir.create(SAVE_PATH)
Sys.setenv(RESAMPLE_PATH = paste0(SAVE_PATH, sep = "_", "resample"))
Sys.setenv(CI_PATH       = paste0(SAVE_PATH, sep = "_", "ci"))
Sys.setenv(PROXY_PATH    = paste0(SAVE_PATH, sep = "_", "proxy"))
Sys.setenv(TRUTH_PATH    = paste0(SAVE_PATH, sep = "_", "truth"))
