library(batchtools)

reg = makeRegistry(file.dir = Sys.getenv("ERROR_PATH_LM"), packages = "batchtools")

batchMap(1, fun = function(i) {
  reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_LM"))
  grepLogs(findDone(), "error|Error")
})
