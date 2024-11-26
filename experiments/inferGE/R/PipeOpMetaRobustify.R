#' @export
PipeOpMetaRobustify = R6::R6Class("PipeOpMetaRobustify",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    initialize = function(id = "metarobustify") {
      super$initialize(
        id = id,
        param_set = paradox::ps(),
        param_vals = list(),
        input = data.table::data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table::data.table(name = "output", train = "Task", predict = "Task")
      )
    }
  ),
  private = list(
    .convert = function(inputs) {
      self$state = list()
      taskin = inputs[[1]]

      if (!anyDuplicated(taskin$row_ids))  return(inputs)

      converter = switch(inputs[[1]]$task_type,
        classif = as_task_classif,
        regr = as_task_regr
      )

      taskout = converter(taskin$data(), target = taskin$target_names, id = taskin$id)
      list(taskout)
    },
    .train = function(inputs) private$.convert(inputs),
    .predict = function(inputs) private$.convert(inputs)
  )
)
