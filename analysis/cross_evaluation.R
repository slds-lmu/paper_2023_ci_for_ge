library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)

xs = lapply(list.files(here("results", "density-estimate"), full.names = TRUE), readRDS)

tbl = map(xs, function(x) {
  name = x$original

  cc = x$crosswise_comparison

  measure_name = if ("classif.ce" %in% colnames(cc))  "classif.ce" else "regr.rmse"

  cc$train_data = ifelse(startsWith(cc$task_id, "orig"), "real", "simulated")
  cc$test_data = ifelse(endsWith(cc$task_id, "orig"), "real", "simulated")

  cc[, list(name = name, avg = mean(get(..measure_name)), se = sd(get(..measure_name)) / sqrt(.N)), by = c("learner_id", "task_id", "train_data", "test_data")]
}) |> rbindlist()

tbl$learner_id = ifelse(tbl$learner_id == "ranger", "ranger", "featureless")

tbl_real = tbl[train_data == "real" & learner_id == "ranger", ]
tbl_real = dcast(tbl_real, train_data + name ~ test_data, value.var = c("avg", "se"))
tbl_real = tbl_real[, c("name", "avg_real", "avg_simulated")]
setnames(tbl_real, c("data", "score_real", "score_simulated"))
tbl_real$score_real = round(tbl_real$score_real, 3)
tbl_real$score_simulated = round(tbl_real$score_simulated, 3)

tbl_sim = tbl[train_data == "simulated" & learner_id == "ranger", ]
tbl_sim = dcast(tbl_sim, train_data + name ~ test_data, value.var = c("avg", "se"))
