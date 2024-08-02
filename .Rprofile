source("renv/activate.R")

# This is where the datasets will be cached

if (dir.exists("/gscratch/sfische6/mlr3oml_cache")) {
  options(mlr3oml.cache = "/gscratch/sfische6/mlr3oml_cache")
} else {
  options(mlr3oml.cache = TRUE)
}

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
}

Sys.setenv(RESAMPLE_PATH = paste0(SAVE_PATH, sep = "_", "resample"))
Sys.setenv(CI_PATH = paste0(SAVE_PATH, sep = "_", "ci"))
Sys.setenv(PROXY_PATH = paste0(SAVE_PATH, sep = "_", "proxy"))
Sys.setenv(TRUTH_PATH = paste0(SAVE_PATH, sep = "_", "truth"))
Sys.setenv(ERROR_PATH = paste0(SAVE_PATH, sep = "_", "error"))
Sys.setenv(MERGE_PATH = paste0(SAVE_PATH, sep = "_", "merge"))
Sys.setenv(FINAL_PATH = paste0(SAVE_PATH, sep = "_", "final"))

Sys.setenv(RESAMPLE_PATH_RUNTIME = paste0(SAVE_PATH, sep = "_", "resample_runtime"))

Sys.setenv(VAR_PATH_AZ = paste0(SAVE_PATH, sep = "_", "var_az"))
Sys.setenv(RESAMPLE_PATH_ABLATION = paste0(SAVE_PATH, sep = "_", "resample_ablation"))

Sys.setenv(RESAMPLE_PATH_MORE = paste0(SAVE_PATH, sep = "_", "resample_more"))
Sys.setenv(RESAMPLE_PATH_CONZ = paste0(SAVE_PATH, sep = "_", "resample_conz"))
Sys.setenv(CI_PATH_CONZ = paste0(SAVE_PATH, sep = "_", "ci_conz"))

Sys.setenv(ABLATION_NCV = paste0(SAVE_PATH, sep = "_", "ablation_ncv"))
Sys.setenv(ABLATION_CORT = paste0(SAVE_PATH, sep = "_", "ablation_cort"))
Sys.setenv(ABLATION_CONZ = paste0(SAVE_PATH, sep = "_", "ablation_conz"))
Sys.setenv(ABLATION_HO = paste0(SAVE_PATH, sep = "_", "ablation_ho"))
Sys.setenv(ABLATION_CV = paste0(SAVE_PATH, sep = "_", "ablation_cv"))
Sys.setenv(ABLATION_NCV_CHEAP = paste0(SAVE_PATH, sep = "_", "ablation_ncv_cheap"))
Sys.setenv(ABLATION_CONZ_CHEAP = paste0(SAVE_PATH, sep = "_", "ablation_conz_cheap"))

Sys.setenv(CI_PATH_LOSSES_ORIGINAL = paste0(SAVE_PATH, sep = "_", "ci_losses_original"))
Sys.setenv(CI_PATH_LOSSES_LM = paste0(SAVE_PATH, sep = "_", "ci_losses_lm"))
Sys.setenv(CI_PATH_LOSSES_RIDGE = paste0(SAVE_PATH, sep = "_", "ci_losses_ridge"))
Sys.setenv(CI_PATH_MORE = paste0(SAVE_PATH, sep = "_", "ci_more"))
Sys.setenv(CI_PATH_MORE2 = paste0(SAVE_PATH, sep = "_", "ci_more2"))

Sys.setenv(TRUTH_PATH_LOSSES_RESAMPLE = paste0(SAVE_PATH, sep = "_", "truth_losses_resample"))
Sys.setenv(TRUTH_PATH_LOSSES_LM = paste0(SAVE_PATH, sep = "_", "truth_losses_lm"))
Sys.setenv(TRUTH_PATH_LOSSES_RIDGE = paste0(SAVE_PATH, sep = "_", "truth_losses_ridge"))
Sys.setenv(TRUTH_PATH_LOSSES_ORIGINAL = paste0(SAVE_PATH, sep = "_", "truth_losses_original"))

rm(SAVE_PATH)
