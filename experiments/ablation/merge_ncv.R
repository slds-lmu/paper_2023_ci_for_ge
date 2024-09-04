library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)

pths = list(
  ncv = "ABLATION_NCV",
  cort = "ABLATION_CORT",
  conz = "ABLATION_CONZ",
  ho = "ABLATION_HO",
  cv = "ABLATION_CV"
)

tbls = list(
  cv = make_tbl(c("cv_5", "cv_10", "cv_25", "cv_50", "cv_75", "cv_100")),
  ho = make_tbl(paste0("holdout_", c("50", "60", "66", "75", "80", "90"))),
  ncv = make_tbl("nested_cv"),
  cort = make_tbl("subsampling_100"),
  conz   = make_tbl("conservative_z_50")
)

tbls$ncv = as.data.table(map(tbls$ncv, function(col) rep(col, each = 200L)))
tbls$cort = as.data.table(map(tbls$cort, function(col) rep(col, each = 11L)))
tbls$ncv = as.data.table(map(tbls$ncv, function(col) rep(col, each = 100L)))

# for holdout and cv, nothing has to be done

merged = map(names(pths), function(method) {
  reg_path = pths[[method]]
  reg = loadRegistry(reg_path)
  tbl = tbls[[method]]

  ids = findDone()[[1L]]

  res = rbindlist(map(ids, loadResult))

  res = cbind(res, tbl)
})
