library(batchtools)
library(mlr3misc)

path_truth = "/gscratch/sfische6/benchmarks/ci_for_ge/truth11"
path_proxy = "/gscratch/sfische6/benchmarks/ci_for_ge/proxy11"
path_ci    = "/gscratch/sfische6/benchmarks/ci_for_ge/ci11"

reg_truth = loadRegistry(Sys.getenv("TRUTH_PATH"), make.default = FALSE)
reg_proxy = loadRegistry(Sys.getenv("PROXY_PATH"), make.default = FALSE)
reg_ci    = loadRegistry(Sys.getenv("CI_PATH"),    make.default = FALSE)

jt_truth = unwrap(getJobTable(reg = reg_truth))
jt_proxy = unwrap(getJobTable(reg = reg_proxy))
jt_ci    = unwrap(getJobTable(reg = reg_ci))

tbl_truth = map_dtr(seq_len(nrow(jt_truth)), function(i) {
  loadResult(i, reg = reg_truth)
}, .fill = TRUE)

tbl_proxy = map_dtr(seq_len(nrow(jt_proxy)), function(i) {
  loadResult(i, reg = reg_proxy)
})

tbl_ci = map_dtr(seq_len(nrow(jt_ci)), function(i) {
  loadResult(i, reg = reg_ci)
}, .fill = TRUE)

final = merge(tbl_proxy, tbl_ci, by = c("task", "size", "repl", "learner", "resampling", "measure"))

final = merge(final, tbl_truth, by = c("task", "size", "repl", "learner", "measure"))

if (nrow(final) != nrow(jt_ci)) stop("something went wrong here, fix this")

