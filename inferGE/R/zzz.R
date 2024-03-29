#' @import R6
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @import checkmate
#' @importFrom stats qnorm qt quantile sd var
#' @importFrom utils getFromNamespace
NULL

lg = mlr3::mlr_reflections$loggers[["mlr3"]]

custom_resamplings = new.env()
custom_measures = new.env()

register_mlr3 = function(...) {
  resamplings = mlr3::mlr_resamplings
  measures = mlr3::mlr_measures
  iwalk(as.list(custom_resamplings), function(x, nm) resamplings$add(nm, x))
  iwalk(as.list(custom_measures), function(x, nm) measures$add(nm, x))
}

register_mlr3pipelines = function(...) {
  mlr_pipeops = mlr3pipelines::mlr_pipeops
  mlr_pipeops$add("metarobustify", PipeOpMetaRobustify)
}


.onLoad = function(libname, pkgname) {
  register_namespace_callback(pkgname, "mlr3", register_mlr3)
  register_namespace_callback(pkgname, "mlr3pipelines", register_mlr3pipelines)
} # nocov end



leanify_package()
