#' @import R6
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @import checkmate
NULL

mlr_task_generators = mlr3::mlr_task_generators

lg = mlr3::mlr_reflections$loggers[["mlr3"]]

custom_resamplings = new.env()

register_mlr3 = function() {
  rsmplings = mlr3::mlr_resamplings
  iwalk(as.list(custom_resamplings), function(x, nm) rsmplings$add(nm, x))
}


.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
  register_namespace_callback(pkgname, "mlr3", register_mlr3)
} # nocov end



leanify_package()
