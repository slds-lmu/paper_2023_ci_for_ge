library(batchtools)

reg = makeRegistry(file.dir = Sys.getenv("ERROR_PATH"), packages = "batchtools")

nms = c(
  "final_resample_runtime",
  "final_resample_ridge",
  "final_resample_more",
  "final_resample_lm",
  "final_resample_conz",
  "final_resample_boot",
  "final_resample_austern",
  "final_resample_ablation",
  "final_resample"
)

batchMap(name = nms, fun = function(name) {
  reg = loadRegistry(paste0("/gscratch/sfische6/benchmarks/ci_for_ge/", name))
  grepLogs(findDone(), "error|Error|fallback|warn")
})
