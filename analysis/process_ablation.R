library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)

tbls = map(1:5, function(i) {
  readRDS(here("results", "ablation", paste0(i, ".rds")))
})

truth = readRDS(here("results", "truth.rds"))
truth = truth[loss %in% c("se", "zero_one"), ]

names(tbls) = c("ncv", "cort", "conz", "ho", "cv")

tbls = imap(tbls, function(tbl, nm) {
  tbl = tbl[!grepl("cv_glmnet", learner)]
  tbl$task_type = ifelse(startsWith(tbl$learner, "regr"), "regr", "classif")
  tbl[, let(
    learner = gsub("regr\\.|classif\\.", "", learner)
  )]
  tbl$learner = map_chr(tbl$learner, function(id) {
    switch(id,
      glmnet = "ridge",
      lm = "linear",
      log_reg = "linear",
      ranger = "ranger",
      rpart = "rpart"
    )
  })
  if (nm == "cort") {
    tbl$ratio = map_dbl(tbl$resampling_name, function(x) {
      switch(x, 
        subsampling_100 = 0.9,
        subsampling_100_80 = 0.9,
        subsampling_100_70 = 0.7
      )
    })
  }
  tbl$dgp = ifelse(tbl$dgp == "phyisiochemical_protein",
    "physiochemical_protein", tbl$dgp)
  
  tbl = tbl[, -c("info", "job.id", "data_id", "reg_path", "group", "resampling_name")]
  tbl
})

truth = truth[, c("size", "repl", "dgp", "learner", "R", "ER")]

tbls = imap(tbls, function(tbl, nm) {
  merge(tbl, truth, all.x = TRUE, by = c("size", "repl", "dgp", "learner"))
})

walk(tbls, function(tbl) {
  stopifnot(sum(is.na(tbl$R)) == 0)
  stopifnot(sum(is.na(tbl$ER)) == 0)
  sds = tbl[, list(sd = sd(R)), by = c("repl", "size", "dgp", "learner")]$sd
  stopifnot(identical(unique(sds), 0))
  sds = tbl[, list(sd = sd(ER)), by = c("size", "dgp", "learner")]$sd
  stopifnot(identical(unique(sds), 0))
})

iwalk(tbls, function(tbl, nm) {
  saveRDS(tbl, here("results", "ablation", paste0(nm, ".rds")))
})

tbls_aggr = imap(tbls, function(tbl, nm) {
  param_vars = switch(nm,
    ncv = "reps_outer",
    cort = c("reps", "ratio"),
    conz = c("outer_reps", "inner_reps"),
    ho = "ratio",
    cv = "folds"
  )
  tbl_aggr = tbl[, list(
    cov_R = mean(lower <= R & upper >= R),
    cov_ER = mean(lower <= ER & upper >= R),
    median_width = median(upper - lower)
  ), by = c("size", "dgp", "learner", param_vars)]

iwalk(tbls_aggr, function(tbl, nm) {
  saveRDS(tbl, here("results", "ablation", paste0(nm, "_aggr.rds")))
})
