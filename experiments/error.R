library(batchtools)

reg = makeRegistry(file.dir = Sys.getenv("ERROR_PATH"), packages = "batchtools")

batchMap(1, fun = function(i) {
  reg = loadRegistry(Sys.getenv("RESAMPLE_PATH"))
  grepLogs(findDone(), "error|Error")
})
