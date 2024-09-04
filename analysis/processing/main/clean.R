library(data.table)
library(here)
library(mlr3misc)

ci = readRDS(here("results", "ci.rds"))
ci_aggr = readRDS(here("results", "ci_aggr.rds"))


f = function(ci) {
  ci$method = as.character(ci$method)

  replace_names <- function(df1, methodcol, df2) {
    # Perform the name replacement by matching old names with new names
    if (!all(df1[[methodcol]] %in% df2$orig_names)) {
      stop("Not all method names are in the replacement table")
    }
    df1[[methodcol]] <- df2$new_names[match(df1[[methodcol]], df2$orig_names)]
    return(df1)
  }


  tbl = read.csv(here("analysis", "MethodNames.csv"))

  # Replace the values from tbl$orig_names with those in tbl$new_names
  # in the column method of the ci data frame
  ci = replace_names(ci, "method", tbl)
  ci$method = as.factor(ci$method)

  setnames(ci, c("measure", "learner", "task"),
    c("loss", "inducer", "dgp"))


  old = c("linear", "ranger", "rpart", "ridge")
  new = c("lm_or_logreg", "random_forest", "decision_tree", "ridge_lm_or_logreg")

  ci$inducer = new[match(ci$inducer, old)]
  ci$inducer = as.factor(ci$inducer)

  ci

}

ci_clean = f(ci)

ablation_files = list.files(here("results", "ablation"))
ablation_files = ablation_files[!grepl("aggr", ablation_files)]
tbls = map(ablation_files, function(file) {
  readRDS(here::here("results", "ablation", file))
})

cols = Reduce(intersect, lapply(tbls, colnames))

# rbindlist the tbls but remove all columns that are not present everywhere
tbl_joined = rbindlist(map(tbls, function(tbl) tbl[, cols, with = FALSE]))

ci_clean = rbindlist(list(ci_clean, tbl_joined), use.names = TRUE)

ci_aggr_clean = ci_clean[, .(
  cov_R = mean(lower <= R & upper >= R),
  cov_ER = mean(lower <= ER & upper >= ER),
  cov_PQ = mean(lower <= PQ & upper >= PQ),
  under_R = mean(upper < R),
  under_ER = mean(upper < ER),
  under_PQ = mean(upper < PQ),

  cov_R_se = sd(lower <= R & upper >= R) / .N,
  cov_ER_se = sd(lower <= ER & upper >= ER) / .N,
  cov_PQ_se = sd(lower <= PQ & upper >= PQ) / .N,
  width = mean(upper - lower),
  width_median = median(upper - lower),
  width_sd = sd(upper - lower),
  bias = mean(estimate - R),
  ER = mean(ER), # due to some numeric instabilities there are can be slight differences between the 'same' ER, so we here take the mean
  # this is only numerics, no bug
  R_sd = sd(R),
  estimate_sd = sd(estimate),
  rmse_ER = sqrt(mean((estimate - mean(R))^2)),
  rmse_R = sqrt(mean((estimate - R)^2)),
  task_type = as.factor(task_type[1]),
  iters = mean(iters)
), by = c("dgp", "inducer", "size", "method",  "loss")]

stopifnot(
  uniqueN(ci_aggr_clean[loss %in% c("winsorized_se", "percentual_ae", "standardized_ae"), .N, by = measure]$N) == 1)

saveRDS(ci_clean, here("results", "clean", "ci.rds"))
saveRDS(ci_aggr_clean, here("results", "clean", "ci_aggr.rds"))
