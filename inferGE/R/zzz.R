#' @import R6
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @import checkmate
NULL

mlr_task_generators = mlr3::mlr_task_generators

lg = mlr3::mlr_reflections$loggers[["mlr3"]]


.onLoad = function(libname, pkgname) {
  mlr_learners = mlr3::mlr_learners
  mlr_learners$add("regr.tabnet", LearnerRegrTabNet)
  mlr_learners$add("classif.tabnet", LearnerClassifTabNet)
  # nocov start
  backports::import(pkgname)
} # nocov end

leanify_package()
