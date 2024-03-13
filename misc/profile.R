library(mlr3)
library(mlr3learners)
library(data.table)
library(inferGE)

mlr3::Task$set("public", "holdout_ratio", NULL, overwrite = TRUE)
mlr3::Task$set("public", "holdout_rows", NULL, overwrite = TRUE)
# don't do this at home
mlr3::Task$set("active", "row_roles", function(rhs) {
  if (missing(rhs)) {
    roro = private$.row_roles
    if (!is.null(self$holdout_ratio)) {
      roro$holdout = sample(self$holdout_rows, size = self$holdout_ratio * length(self$holdout_rows))
    }

    return(roro)
  }

  assert_has_backend(self)
  assert_list(rhs, .var.name = "row_roles")
  assert_names(names(rhs), "unique", permutation.of = mlr_reflections$task_row_roles, .var.name = "names of row_roles")
  rhs = map(rhs, assert_row_ids, .var.name = "elements of row_roles")

  private$.hash = NULL
  private$.row_roles = rhs
}, overwrite = TRUE)

beta = matrix(rnorm(10), ncol = 1)
X = matrix(rnorm((100000L + 200L) * 10), ncol = 10)
y = X %*% beta

dt = as.data.table(cbind(y, X))
colnames(dt) = c("y", paste0("x", 1:10))

task = as_task_regr(dt, target = "y")

learner = lrn("regr.lm", predict_sets = c("test", "holdout"))

task$row_roles$holdout = 1:100000L
task$row_roles$use = 100001L:100200L

resampling = rsmp("nested_cv", folds = 5, repeats = 200L)
resampling$instantiate(task)
resampling$iters

options(mlr3.debug = TRUE)

lgr::get_logger("mlr3")$set_threshold("warn")

# profvis::profvis({
#   resample(
#     task,
#     learner,
#     rsmp("nested_cv", folds = 5, repeats = 200L)
#   )
# })

holdout = function() {
  lgr::get_logger("mlr3")$set_threshold("warn")
  learner = lrn("regr.ranger", num.trees = 50L, predict_sets = c("test", "holdout"))

  resampling = rsmp("nested_cv", folds = 5, repeats = 1L)
  task = task$clone(deep = TRUE)
  resampling$instantiate(task)
  resample(
    task,
    learner,
    resampling
  )
}

lessholdout = function() {
  lgr::get_logger("mlr3")$set_threshold("warn")
  learner = lrn("regr.ranger", num.trees = 50L, predict_sets = c("test", "holdout"))

  # don't do this at homrr = e
  resampling = rsmp("nested_cv", folds = 5, repeats = 1L)
  task = task$clone(deep = TRUE)
  resampling$instantiate(task)
  task$holdout_rows = 1:100000L
  task$holdout_ratio = 1 / resampling$iters

  rr = resample(
    task,
    learner,
    resampling
  )
}

rr = lessholdout()

bench::mark(
  less = lessholdout(),
  holdout = holdout(),
  check = FALSE,
  iterations = 1
)


