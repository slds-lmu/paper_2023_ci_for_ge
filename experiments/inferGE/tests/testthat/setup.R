lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
old_plan = future::plan()
lg$set_threshold("warn")

library(testthat)
library(checkmate)
library(inferGE)
library(mlr3)
library(mlr3pipelines)
