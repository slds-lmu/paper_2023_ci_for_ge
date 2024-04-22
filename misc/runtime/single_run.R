library(mlr3oml)
library(mlr3verse)
library(bench)

ps = c("test", "holdout")

# Part 1: cv_glmnet
#
# a) How much do we save, when we use CV glmnet with 3 folds

generator = tgen("friedman1")

task = generator$generate(n = 105000)

task$row_roles$use = 1:5000
task$row_roles$holdout = 5001:105000

cv_glm3 = lrn("regr.cv_glmnet", nfolds = 3, predict_sets = ps, alpha = 0)
cv_glm10 = lrn("regr.cv_glmnet", nfolds = 10, predict_sets = ps, alpha = 0)

res_cv_3_10 = bench::mark(
  cv_glm3 = resample(task, cv_glm3, rsmp("holdout")),
  cv_glm10 = resample(task, cv_glm10, rsmp("holdout")),
  check = FALSE
)

# factor of around 0.85 --> not really a lot to gain
# --> we can stick with 10 folds


# b) How much do we save when going from 100_000 -> 50_000

task_100000 = task$clone(deep = TRUE)
task_50000 = task$clone(deep = TRUE)
task_50000$row_roles$holdout = 5001:55000

res_cv_task_50_100 = bench::mark(
  cv_task_1000000 = resample(task_100000, cv_glm10, rsmp("holdout")),
  cv_task_500000 = resample(task_50000, cv_glm10, rsmp("holdout")),
  check = FALSE
)

# -> for glmnet less predictions is not so important


# Part 2: ranger

ranger50 = lrn("regr.ranger", num.trees = 50, predict_sets = ps)
ranger100 = lrn("regr.ranger", num.trees = 100, predict_sets = ps)


res_cv_3_10 = bench::mark(
  ranger50 = resample(task, ranger50, rsmp("holdout")),
  ranger100 = resample(task, ranger100, rsmp("holdout")),
  check = FALSE
)

# -> Scales almost lineary -> reduce to 50 trees if noone disagrees.

res_ranger_task_50_100 = bench::mark(
  cv_task_1000000 = resample(task_100000, ranger50, rsmp("holdout")),
  cv_task_500000 = resample(task_50000, ranger50, rsmp("holdout")),
  check = FALSE
)
 600  / 960



# -> 40% reduction in runtime for ranger
# -> I think we should do this

# Part 3: rpart

rpart3 = lrn("regr.rpart", xval = 3, predict_sets = ps)
rpart10 = lrn("regr.rpart", xval = 10, predict_sets = ps)

res_rpart_3_10 = bench::mark(
  ranger50 = resample(task, rpart3, rsmp("holdout")),
  ranger100 = resample(task, rpart10, rsmp("holdout")),
  check = FALSE
)

# -> 70 / 90 -> 22 % runtime reduction
# I guess we can do it

res_rpart_task_50_100 = bench::mark(
  rpart_task_1000000 = resample(task_100000, rpart3, rsmp("holdout")),
  rpart_task_500000 = resample(task_50000, rpart3, rsmp("holdout")),
  check = FALSE
)

50 / 70


# -> 30 % reduction in runtime



## Is classif really slower than regr???



# train vs predict


trained_ranger = lrn("regr.ranger", num.trees = 50, predict_sets = ps)$train(task_100000)
res_ranger_train_predict = bench::mark(
  train = ranger50$train(task_100000),
  predict = trained_ranger$predict(task_100000, row_ids = 1:105000),
  check = FALSE
)

rpart = lrn("regr.rpart", predict_sets = ps, xval = 10)
trained_rpart = lrn("regr.rpart", xval = 10, predict_sets = ps)$train(task_100000)
res_rpart_train_predict = bench::mark(
  train = rpart$train(task_100000),
  predict = trained_rpart$predict(task_100000, row_ids = 1:105000),
  check = FALSE
)

glmnet = lrn("regr.cv_glmnet", predict_sets = ps, nfolds = 10)
trained_glmnet = lrn("regr.cv_glmnet", nfolds = 10, predict_sets = ps)$train(task_100000)
res_glmnet_train_predict = bench::mark(
  train = glmnet$train(task_100000),
  predict = trained_glmnet$predict(task_100000, row_ids = 1:105000),
  check = FALSE
)
res_glmnet_train_predict
