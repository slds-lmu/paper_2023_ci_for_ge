library(data.table)
library(here)
library(ggplot2)
library(mlr3misc)

ci = readRDS(here("results", "ci_small.rds"))
setnames(ci, c("task"), c("dgp"))

ci = ci[method == "holdout_66" & measure %in% c("se", "zero_one"),
  c("repl", "size", "learner", "dgp", "R")
]


f = function(name) {
  data = readRDS(here("results", "ablation_orig", "results", paste0(name, ".rds")))
  #data = readRDS(here("results", "ablation_orig", paste0(name, ".rds")))
  data = data[!grepl("cv_glmnet", learner)]
  data[dgp == "phyisiochemical_protein", let(dgp = "physiochemical_protein")]

  
  f = function(id) {
    switch(id,
           regr.rpart = "rpart",
           classif.rpart = "rpart",
           regr.ranger = "ranger",
           classif.ranger = "ranger",
           regr.lm = "linear",
           classif.log_reg = "linear",
           regr.glmnet = "ridge",
           classif.glmnet = "ridge"
    )
  }
  data[, let(learner = map_chr(learner, f))]

  data$job.id = NULL
  data$data_id = NULL
  data$resampling_name = NULL
  data$reg_path = NULL
  data$group = NULL

  data = merge(data, ci, by = c("repl", "size", "dgp", "learner"))
  data
}
ds = map(1:5, f)

names(ds) = c("ncv", "corrected_t", "conservative_z", "holdout", "cv")

iwalk(ds, function(d, nm) {
  saveRDS(d, here("results", "ablation", paste0(nm, ".rds")))
})

aggrs = imap(ds, function(d, nm) {
  d[, let(
    ER = mean(R),
    width = upper - lower
  ), by = c("dgp", "learner", "size")]

  param_columns = switch(nm,
    ncv = "reps_outer",
    holdout = "ratio",
    cv = "folds",
    corrected_t = "reps",
    conservative_z = c("outer_reps", "inner_reps")
  )

  d = d[, list(
    cov_R = mean(lower <= R & upper >= R),
    cov_ER = mean(lower <= ER & upper >= R),
    median_width = median(width)
  ), by = c("dgp", "learner", "size", param_columns)]

  if (nm == "conservative_z") {
    setnames(d, c("outer_reps", "inner_reps"), c("reps_outer", "reps_inner"))
  }
  d
})

iwalk(aggrs, function(d, nm) {
  saveRDS(d, here("results", "ablation", paste0(nm, "_aggr.rds")))
})
