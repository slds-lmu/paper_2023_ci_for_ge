library(mlr3)
library(data.table)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3oml)
library(inferGE)
library(mlr3misc)
library(DBI)
library(here)
source(here("experiments", "helper.R"))

set.seed(42)

TASKS = list(
  classif = c(45570, 45689, 45704, 45654, 45665, 45668, 45669, 45672, 45693),
  regr = c(45692, 45694, 45655, 45666, 45667, 45670, 45671, 45695, 45696)
)

SIZES = c(100L, 500L, 1000L, 5000L, 10000L)

lambdas = map(SIZES, function(size) {
  res = imap(TASKS, function(task_ids, task_type) {
    lambdas = map_dbl(task_ids, function(tid) {
      # any resampling for which we don't compute a proxy quantity
      task = make_task(tid, size, 1, rsmp("nested_cv"))
      base_lrn = lrn(paste0(task_type, ".cv_glmnet"), alpha = 0, nfolds = 10L)
      glrn = as_learner(ppl("robustify", task = task, learner = base_lrn) %>>% base_lrn)
      glrn$train(task)
      glrn$model[[paste0(task_type, ".cv_glmnet")]]$model$lambda.min
    })

    data.table(
      lambda = lambdas,
      task_id = task_ids,
      size = size
    )
  }) |> rbindlist()
}) |> rbindlist()

lambdas$name = map_chr(lambdas$task_id, function(id) odt(id)$name)
saveRDS(lambdas, here("data", "lambdas.R"))
