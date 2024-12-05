library(mlr3torch)
library(mlr3tuning)
library(mlr3mbo)
library(mlr3learners)
source("experiments/helper.R")

set.seed(1)
task = tsk("spam")
task = po("encode")$train(list(task))[[1L]]
r = rsmp("holdout")

pars = list(patience = 20L, batch_size = 512, drop_last = FALSE)
l = make_learner(learner_id = "classif.mlp", learner_name = "mlp", task = task, resampling = r, learner_params = pars)

t0 = Sys.time()
l$train(task)
t = Sys.time() - t0
print(t)

l1 = make_learner(learner_id = "classif.xgboost", learner_name = "xgboost", task = task, resampling = r, learner_params = list(early_stopping_rounds = 20L, eval_metric = "mlogloss"))

t1 = Sys.time()
l1$train(task)
t2 = Sys.time() - t1
print(t2)