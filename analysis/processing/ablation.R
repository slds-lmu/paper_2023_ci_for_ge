library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)

files = list.files(here("results", "raw"))
files = files[endsWith(files, "_orig.rds")]
nms = gsub("_orig.rds", "", files)

tbls = map(files, function(file) {
  readRDS(here("results", "raw", file))
})

names(tbls) = nms

if (!file.exists(here("results", "main", "truth.rds"))) {
  stopf("Run analysis/processing/truth_losses.rds first.")
}

truth = readRDS(here("results", "main", "truth.rds"))
truth = truth[loss %in% c("se", "zero_one"), ]

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
        subsampling_100_80 = 0.8,
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
  sds = tbl[, list(sd = sd(ER)), by = c("size", "dgp", "learner")]$sd
  stopifnot(identical(unique(sds), 0))
})


tbls$cort_best$ratio = 0.9
tbls$conz_cheap = rbindlist(list(tbls$conz_cheap, tbls$conz_cheap_best))
tbls$cort = rbindlist(list(tbls$cort, tbls$cort_best))


tbls$conz_cheap_best = NULL
tbls$cort_best = NULL

tbls = imap(tbls, function(tbl, nm) {

  if (nm %in% c("ncv", "ncv_cheap")) {
    tbl$method = paste0("ncv_", tbl$reps_outer, "_", 5)
    tbl$iters = tbl$reps_outer * 5^2
  }

  if (nm %in% c("cort", "cort_cheap")) {
    tbl$method = paste0("cort_", tbl$reps, "_", tbl$ratio * 100)
    tbl$iters = tbl$reps
  }
  if (nm %in% c("conz", "conz_cheap")) {
    tbl$method = paste0("conz_", tbl$outer_reps, "_", tbl$inner_reps)
    tbl$iters = tbl$inner_reps * (2 * tbl$outer_reps + 1)
  }
  if (nm == "ho") {
    ratio = as.character(floor(tbl$ratio  * 100))
    tbl$method = paste0("ho_", ratio)
    tbl$iters = 1
  }
  if (nm == "cv") {
    tbl$method = paste0("cv_", tbl$folds)
    tbl$iters = tbl$folds
  }
  tbl$loss = ifelse(tbl$task_type == "regr", "se", "zero_one")
  setnames(tbl,
    c("learner"),
    c("inducer")
  )
  tbl$PQ = NA_real_

  old = c("linear", "ranger", "rpart", "ridge")
  new = c("lm_or_logreg", "random_forest", "decision_tree", "ridge_lm_or_logreg")

  tbl$inducer = new[match(tbl$inducer, old)]
  tbl$inducer = as.factor(tbl$inducer)

  tbl
})

tbls_aggr = imap(tbls, function(tbl, nm) {
  param_vars = switch(nm,
    ncv = "reps_outer",
    cort = c("reps", "ratio"),
    conz = c("outer_reps", "inner_reps"),
    ho = "ratio",
    cv = "folds",
    conz_cheap = c("outer_reps", "inner_reps"),
    cort_cheap = c("reps", "ratio"),
    ncv_cheap = "reps_outer"
  )
  tbl_aggr = tbl[, list(
    cov_R = mean(lower <= R & upper >= R),
    cov_ER = mean(lower <= ER & upper >= ER),
    median_width = median(upper - lower),
    mean_width = mean(upper - lower),
    sd_width = sd(upper - lower),
    task_type = task_type[1]
  ), by = c("size", "dgp", "inducer", param_vars)]
  tbl_aggr
})

iwalk(tbls_aggr, function(tbl, nm) {
  saveRDS(tbl, here("results", "ablation", paste0(nm, "_aggr.rds")))
})

iwalk(tbls, function(tbl, nm) {
  saveRDS(tbl, here("results", "ablation", paste0(nm, ".rds")))
})
