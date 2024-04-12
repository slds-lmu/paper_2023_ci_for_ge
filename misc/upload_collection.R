devtools::load_all("~/mlr/mlr3oml")

dt = read.csv("~/gh/paper_2023_ci_for_ge/misc/data_ids.csv")
data_ids = dt$data_id
task_types =  dt$task_type

task_ids = list()

for (i in seq_along(data_ids)) {

  type = task_types[i]
  id = data_ids[i]
  odata = odt(id)

  if (type == "regr") {
    estimation_procedure = 7
  } else {
    estimation_procedure = 1
  }

  task_id = publish_task(
    id = data_ids[i],
    type = task_types[i],
    estimation_procedure = estimation_procedure,
    target = odata$target_names

  )

  task_ids = append(task_ids, task_id)
}

publish_collection(
  unlist(task_ids),
  name = "CI for GE benchmark",
  desc = paste0(
    "This is a collection of datasets that can be used to evaluate Confidence Interval methods for the Generalization Error.",
    "The task splits can be ignored.",
    "For more information, see [https://github.com/slds-lmu/paper_2023_ci_for_ge](https://github.com/slds-lmu/paper_2023_ci_for_ge)."
  ),
  test_server = FALSE
)
