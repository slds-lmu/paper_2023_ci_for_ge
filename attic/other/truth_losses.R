library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)

truth = readRDS(here("results", "truth_losses.rds"))
is_regr = map_lgl(truth$dt, function(x) length(x) == 5)
truth_regr = truth[is_regr, ]
truth_regr = cbind(truth_regr, rbindlist(truth_regr$dt))
truth_regr$dt = NULL
truth_regr = melt(truth_regr,
  id.vars = c("size", "repl", "learner_name", "task_name"),
  measure.vars = c("se", "ae", "standardized_ae", "percentual_ae", "winsorized_se"),
  variable.name = "loss",
  value.name = "R"
  )
truth_regr$task_type = "regr"
truth_classif = truth[!is_regr, ]
truth_classif = cbind(truth_classif, rbindlist(truth_classif$dt))
truth_classif$dt = NULL
truth_classif = melt(truth_classif,
  id.vars = c("size", "repl", "learner_name", "task_name"),
  measure.vars = c("zero_one", "bbrier", "logloss"),
  variable.name = "loss",
  value.name = "R"
)
truth_classif$task_type = "classif"
truth = rbind(truth_regr, truth_classif)
setnames(truth, c("task_name", "learner_name"), c("dgp", "learner"))
truth[, let(
  ER = mean(R)
), by = c("dgp", "learner", "size", "loss")]


saveRDS(truth, here("results", "truth.rds"))
