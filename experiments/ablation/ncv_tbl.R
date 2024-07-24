library(batchtools)
library(devtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(inferGE)
library(here)


source(here("experiments", "ablation", "helper.R"))
tbl = make_tbl("nested_cv")
saveRDS(tbl, "~/ncv_tbl.rds")

