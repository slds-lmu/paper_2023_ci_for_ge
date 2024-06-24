library(batchtools)
reg = makeRegistry("/gscratch/sfische6/benchmarks/ci_for_ge/merge_ablation", packages = c("batchtools", "data.table"))

f = function(path) {
  reg = loadRegistry(path)
  ids = findDone()[[1]]
  rbindlist(lapply(ids, loadResult))
}

paths = paste0("/gscratch/sfische6/benchmarks/ci_for_ge/final_ablation_",
  c("conz", "ncv", "cv", "ho", "cort"))

batchMap(f = f, path = paths)

