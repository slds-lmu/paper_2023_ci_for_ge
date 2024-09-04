library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)


# final.rds is the result of analysis/process/merge_oob.R

ci = readRDS(here("results", "final.rds"))
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
truth = readRDS(here("results", "truth.rds"))
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
small_cheap = readRDS(here("results", "small_cheap.rds"))
small_cheap$info = NULL
small_cheap = merge(small_cheap, truth[, c("repl", "size", "learner", "task", "measure", "R")],
  by = c("repl", "size", "learner", "task", "measure"), all.x = TRUE)
small_cheap$PQ = NA
small_cheap$method = paste0(small_cheap$resampling, "_250")
small_cheap$resampling = NULL

# the good performing methods that we then evaluated on even more losses:
good_losses = readRDS(here("results", "good_losses.rds"))
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
conz = readRDS(here("results", "ci_conz.rds"))
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

saveRDS(ci, here("results", "ci.rds"))


