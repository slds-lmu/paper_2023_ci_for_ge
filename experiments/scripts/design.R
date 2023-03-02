# This file creates the experiment design

library(mlr3)

learners_regr = list(
  lrn("regr.lm")
)

learners_classif = list(
  lrn("classif.log_reg")
)
