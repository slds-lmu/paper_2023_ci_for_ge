library(batchtools)
library(mlr3misc)

PATH_TRUTH = "/gscratch/sfische6/benchmarks/ci_for_ge/truth"
path_ci = "/gscratch/sfische6/benchmarks/ci_for_ge/eval30"

reg_truth = loadRegistry(PATH_TRUTH, make.default = FALSE)
reg_ci = loadRegistry(PATH_CI, make.default = FALSE)

jt_truth = unwrap(getJobTable(reg = reg_truth))
jt_ci = unwrap(getJobTable(reg = reg_ci))

jt_truth = map_dtr(seq_len(nrow(jt_truth), function(i) {
    loadResult(i)
})

jt_truth = map_dtr(seq_len(nrow(jt_truth), function(i) {
    loadResult(i)
})
