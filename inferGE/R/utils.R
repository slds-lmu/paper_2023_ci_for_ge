default_loss_fn = function(task_type) {
  loss_fn = switch(task_type,
    classif = list(zero_one = mlr3measures::zero_one),
    regr = list(se = mlr3measures::se),
    stopf("Task type '%s' currently not supported.", task_type)
  )
}

assert_alpha = function(x) {
  assert_numeric(x, len = 1L, lower = 0, upper = 1)
}
