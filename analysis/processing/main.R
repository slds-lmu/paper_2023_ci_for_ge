library(here)
library(data.table)
library(mlr3misc)

# inputs:
# ci_for_ge.rds is final_final/1.rds
# oob_632.rds is the rbindlist result from final_ci_boot/results/*.rds
# oob.rds is the rbindlist result from final_ci_boot_old/results/*.rds
# ridge.rds is from final_final_ridge/results/1.rds
# lm.rds is from final_final_lm/results/1.rds

ci = readRDS(here("results", "raw", "ci_for_ge.rds"))
oob = readRDS(here("results", "raw", "oob.rds"))
oob_632 = readRDS(here("results", "raw", "oob_632.rds"))
ridge = readRDS(here("results", "raw", "ridge.rds"))
lm = readRDS(here("results", "raw", "lm.rds"))
truth = readRDS(here("results", "main", "truth.rds"))


oob_632$info = list(list(NULL))
oob = rbindlist(list(oob, oob_632), use.names = TRUE)

setnames(oob, "resampling", "proxy_resampling")
oob$info = list(list(NULL))
ci_oob = merge(oob,
  ci[method == "oob_100", c("measure", "learner", "task", "size", "repl", "R", "PQ")],
  by = c("measure", "learner", "task", "size", "repl")
)

final = rbindlist(list(ci, ci_oob, ridge, lm), use.names = TRUE)

final$method = ifelse(final$method == "diettrich", "dietterich", final$method)
final$task = ifelse(final$task == "phyisiochemical_protein", "physiochemical_protein", final$task)
final$learner = ifelse(final$learner == "ridge", "ridge_tuned",
  ifelse(final$learner == "ridge2", "ridge", final$learner))

final[startsWith(method, "austern_zhou"), let(
  lower = estimate - ((upper - lower) / 2) / sqrt(2),
  upper = estimate + ((upper - lower) / 2) / sqrt(2),
  info = mlr3misc::map(info, function(i) mlr3misc::insert_named(i, list(correct = TRUE, s2_cv = i$s2_cv / 2)))
)]

#saveRDS(final, here("results", "final.rds"))

ci = final
# process ci

# ridge_tuned was the cv_glmnet that we later replaced by first finding lambda before
# the experiments and then using a fixed one
# for the conservative_z, there was a bug in the implemented for the first round, so we also remove it
# the percentual_se and standardized_se were later replaced by percentual_ae and standardized_ae
ci = ci[learner != "ridge_tuned" & method != "conzervative_z" & measure %nin% c("percentual_se", "standardized_se"), ]
ci$info = NULL
ci$proxy_resampling = NULL

ci[,stopifnot(uniqueN(R) == 1), by = c("task", "learner", "repl", "size", "measure")]

truth_old = ci[, list(R = R[[1L]]), by = c("task", "learner", "repl", "size", "measure", "task_type")]

# truth was the result of experiments/truth_loss/merge.R
# truth was obtained some time later, where the R version on the cluster changed
# and likely some other system dependencies.
# the "problem" is that this changed the values of the truths, which is illustrated
# in the next lines
# note that this only was a problem for the instable combination of adult + linear,
# as well as a few cases of linear and breast or video transcoding for small n (also very unstable),
# for everything else it was almost the same (1e-10 range).
# This really seems to be a numeric issue
setnames(truth, c("loss", "dgp"), c("measure", "task"))

# here we inspect the differences between the results
tmp = merge(truth, truth_old, by = c("task", "learner", "repl", "size", "measure"))
tmp$dif = abs(tmp$R.x - tmp$R.y)
tmp[order(dif, decreasing = TRUE), c("dif", "R.x", "R.y", "task", "learner", "repl", "size", "measure")]
tmp[dif / ((R.x + R.y) / 2) > 0.1, ]$task

# but, the expected risks are as expected, so should really not be a bug
truth_old[learner == "linear" & task == "adult" & measure == "zero_one", mean(R), by = "size"]
truth[learner == "linear" & task == "adult" & measure == "zero_one", mean(R), by = "size"]

# seems like between the two runs, something on the cluster changed that made the results non-reproducible, even though
# we set the seeds. For this reason, we use the "old" truths for everything but the newly generated truths
# (winsorized_se, percentual_ae, standardized_ae)

truth = truth[measure %in% c("winsorized_se", "percentual_ae", "standardized_ae"), -"ER"]
truth = rbind(truth_old, truth)
truth[, let(ER = mean(R)), by = c("learner", "size", "task", "measure")]

# methods NCV and ConZ that were ran cheaper on all sizes
small_cheap = readRDS(here("results", "raw", "small_cheap.rds"))
small_cheap$info = NULL
small_cheap = merge(small_cheap, truth[, c("repl", "size", "learner", "task", "measure", "R")],
  by = c("repl", "size", "learner", "task", "measure"), all.x = TRUE)
small_cheap$PQ = NA
small_cheap$method = paste0(small_cheap$resampling, "_250")
small_cheap$resampling = NULL

# the good performing methods that we then evaluated on even more losses:
good_losses = readRDS(here("results", "raw", "good_losses.rds"))
good_losses
good_losses$task = ifelse(good_losses$task == "phyisiochemical_protein",
  "physiochemical_protein", good_losses$task)
good_losses$info = NULL
good_losses$learner = ifelse(good_losses$learner == "ridge2", "ridge", good_losses$learner)
good_losses$resampling = NULL
good_losses$PQ = NA
good_losses = merge(good_losses, truth[, c("repl", "size", "learner", "task", "measure", "R")],
  by = c("repl", "size", "learner", "task", "measure"), all.x = TRUE)



# prepare conz
conz = readRDS(here("results", "raw", "ci_conz.rds"))
conz$method = "conservative_z"
conz$iters = 315
conz$PQ = NA
conz = merge(conz, truth[, c("repl", "size", "learner", "task", "measure", "R")],
  by = c("repl", "size", "learner", "task", "measure"), all.x = TRUE)
conz$resampling = NULL

stopifnot(all.equal(conz[learner == "linear" & repl == 1 & size == 100 & task == "adult" & measure == "bbrier", ]$R,
          unique(ci[learner == "linear" & repl == 1 & size == 100 & task == "adult" & measure == "bbrier", ]$R)))

setdiff(colnames(conz), colnames(ci))
setdiff(colnames(ci), colnames(conz))

ci = rbind(conz, ci, small_cheap, good_losses)

ci[, let(
  task = as.factor(task),
  learner = as.factor(learner),
  measure = as.factor(measure),
  method = as.factor(method)
)]


ci[, let(
  ER = mean(R)
), by = c("task", "learner", "size", "method", "measure")]


ci$measure |> table()
stopifnot(length(unique(table(ci$learner))) == 1)
stopifnot(length(unique(ci$measure)) == 8)
stopifnot(length(unique(table(ci$task))) == 2)
stopifnot(length(unique(table(ci$repl))) == 1)

stopifnot(sum(is.na(ci$lower)) == 1) # 632plus on small data
impute = ci[learner == "ridge" & size == 100 & measure == "zero_one" & method == "632plus_10" & task == "colon",
   list(lower = mean(lower, na.rm = TRUE), upper = mean(upper, na.rm = TRUE))]
ci[is.na(lower), c("lower", "upper")] = impute


# check that risk merging etc. did not go wrong
tmp = ci[, list(R = mean(R), sd = sd(R)), by = c("learner", "task", "measure", "repl", "size")][order(sd, decreasing = TRUE)]
stopifnot(nrow(tmp[sd != 0, ]) == 0)

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
  uniqueN(ci_aggr_clean[loss %in% c("winsorized_se", "percentual_ae", "standardized_ae"), .N, by = loss]$N) == 1)

saveRDS(ci_clean, here("results", "main", "ci.rds"))
saveRDS(ci_aggr_clean, here("results", "main", "ci_aggr.rds"))
