library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)


source(here("experiments", "ablation", "helper.R"))
#tbl = make_tbl("nested_cv")

reg = makeRegistry("/gscratch/sfische6/benchamrks_ci_for_ge/mrg2", packages = c("data.table", "batchtools", "mlr3misc"))

merged = batchMap(method = 1, function(method) {
  reg = loadRegistry(Sys.getenv("ABLATION_NCV"))
  ids = findDone()[[1L]]

  rbindlist(map(ids, loadResult))
})

submitJobs()
