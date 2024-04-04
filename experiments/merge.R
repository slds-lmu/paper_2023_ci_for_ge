library(batchtools)
library(mlr3misc)

path_truth = "/gscratch/sfische6/benchmarks/ci_for_ge/truth11"
path_proxy = "/gscratch/sfische6/benchmarks/ci_for_ge/proxy11"
path_ci    = "/gscratch/sfische6/benchmarks/ci_for_ge/ci11"

reg_truth = loadRegistry(path_truth, make.default = FALSE)
reg_proxy = loadRegistry(path_proxy, make.default = FALSE)
reg_ci    = loadRegistry(path_ci,    make.default = FALSE)

jt_truth = unwrap(getJobTable(reg = reg_truth))
jt_proxy = unwrap(getJobTable(reg = reg_proxy))
#jt_ci    = unwrap(getJobTable(reg = reg_ci))

tbl_truth = map_dtr(seq_len(nrow(jt_truth)), function(i) {
  loadResult(i, reg = reg_truth)
}, .fill = TRUE)

tbl_proxy = map_dtr(seq_len(nrow(jt_proxy)), function(i) {
  loadResult(i, reg = reg_proxy)
})

#tbl_ci = map_dtr(seq_len(nrow(jt_ci)), function(i) {
#  loadResult(i, reg = reg_ci)
#}, .fill = TRUE)
