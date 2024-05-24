library(batchtools)

reg = makeRegistry(file.dir = Sys.getenv("ERROR_PATH_RIDGE"), packages = "batchtools")

batchMap(1, fun = function(i) {
  reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_RIDGE"))
  grepLogs(findDone(), "error|Error")
})
