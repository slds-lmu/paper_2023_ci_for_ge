library(ggplot2)
library(data.table)
library(here)
library(mlr3misc)

tbl = readRDS(here("results", "raw", "runtime.rds"))

tbl <- dcast(tbl, method + inducer ~ size, value.var = "time")

xtable::xtable(tbl[inducer == "random_forest", -"inducer"])