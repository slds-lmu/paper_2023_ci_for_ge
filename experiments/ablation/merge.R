library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)


source(here("experiments", "ablation", "helper.R"))

methods = c("ncv", "cort", "conz", "ho", "cv")


reg = makeRegistry("/gscratch/sfische6/benchmarks/ci_for_ge/final_ablation_merge", packages = c("data.table", "batchtools", "mlr3misc"))
batchExport(list(make_tbl = make_tbl))

merged = batchMap(method = methods, function(method) {
pth = switch(method,
  ncv = "ABLATION_NCV",
  cort = "ABLATION_CORT",
  conz = "ABLATION_CONZ",
  ho = "ABLATION_HO",
  cv = "ABLATION_CV"
)

tbl = switch(method,
  cv = make_tbl(c("cv_5", "cv_10", "cv_25", "cv_50", "cv_75", "cv_100")),
  ho = make_tbl(paste0("holdout_", c("50", "60", "66", "75", "80", "90"))),
  ncv = make_tbl("nested_cv"),
  cort = make_tbl(c("subsampling_100", "subsampling_100_80", "subsampling_100_70")),
  conz   = make_tbl("conservative_z_50")
)

if (method == "ncv") tbl = as.data.table(map(tbl, function(col) rep(col, each = 20L)))
if (method == "cort") tbl = as.data.table(map(tbl, function(col) rep(col, each = 11L)))
if (method == "conz") tbl = as.data.table(map(tbl, function(col) rep(col, each = 100L)))

  reg = loadRegistry(Sys.getenv(pth))

  ids = findDone()[[1L]]

  res = rbindlist(map(ids, loadResult))
  if (nrow(res) != nrow(tbl)) stop("bug")

  res = cbind(res, tbl)
})

submitJobs(resources = list(memory = 512 * 256, clusters = "moran-bigmem"))
