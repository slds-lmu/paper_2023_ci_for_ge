library(batchtools)

reg = makeRegistry(Sys.getenv("FINAL_PATH"), packages = c("batchtools", "data.table"))

batchMap(i = 1, function(i) {
  reg = loadRegistry(Sys.getenv("MERGE_PATH"))
  rbindlist(lapply(1:281, loadResult), fill = TRUE)
})
