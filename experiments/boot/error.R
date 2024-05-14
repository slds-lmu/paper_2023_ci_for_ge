library(batchtools)

reg = makeRegistry(file.dir = Sys.getenv("ERROR_PATH_BOOT"), packages = "batchtools")

batchMap(1, fun = function(i) {
  reg = loadRegistry(Sys.getenv("RESAMPLE_PATH_BOOT"))
  grepLogs(findDone(), "error|Error")
})
