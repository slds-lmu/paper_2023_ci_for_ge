# the ids here are generic, i.e. they will be translated to clasisf and regr accordingly
learner_ids = list(
  ranger_10 = list(
    classif = list(id = "ranger", params = list(num.trees = 10)),
    regr    = list(id = "ranger", params = list(num.trees = 10)),
  ),
  ranger_100 = list(
    classif = list(id = "ranger", params = list(num.trees = 100)),
    regr    = list(id = "ranger", params = list(num.trees = 100)),
  ),
  linear = list(
    classif = list(id = "log_reg", params = list()),
    regr    = list(id = "lm", params = list())
  ),
  linear_penalized = list(
    classif = list(id = "cv_glmnet", params = list()),
    regr    = list(id = "cv_glmnet", params = list())
  )
)

make_learner = function(x, task_type) {
  do.call(lrn, args = c(list(id = x$id), x$params))

  if (task_type == "regr") {
    learner = switch(id,
      linear = lrn("regr.lm"),
      ranger_

      stop("Unknown learner id")
    )
  } else if (task_type == "classif") {
    learner = switch(id,
      linear = lrn("")
      stop("Unknown learner id")
    )

    if ("prob" %in% learner$predict_types) {
      learner$predict_type = "prob"
    }
  } else {
    stop("Unsupported task type")
  }

  graph = po("")


}