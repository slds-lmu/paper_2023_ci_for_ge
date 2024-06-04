library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)

ci = readRDS(here("results", "final.rds"))
ci = ci[learner != "ridge_tuned", ]

# We observed one NA, which we just remove

#stopifnot(sum(is.na(ci$lower) | is.na(ci$upper) | is.na(ci$estimate)) == 1)

ci = ci[!is.na(estimate), ]

#For now, only look at a subset:

ci[, let(
  task = as.factor(task),
  learner = as.factor(learner),
  measure = as.factor(measure),
  method = as.factor(method),
  iters = NULL
)]


ci[, let(
  ER = mean(R)
), by = c("task", "learner", "size", "method", "measure")]

ci_aggr = ci[, .(
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
  width_sd = sd(upper - lower),
  bias = mean(estimate - R),
  ER = if (uniqueN(ER) == 1) unique(ER) else stop(),
  R_sd = sd(R),
  estimate_sd = sd(estimate),
  task_type = as.factor(task_type[1]),
  iters = iters 
), by = c("task", "learner", "size", "method",  "measure")]

removed_methods = c(
    paste0("oob_", c(10, 50, 100, 500)),
    paste0("632plus_", c(10, 50, 100, 500)),
    "bayle_5_within",
    "bayle_5_all_pairs",
    "bayle_10_within",
    "ls_bootstrap_50",
    "corrected_t_50"
)
ci_aggr_small = ci_aggr[method %nin% removed_methods]
ci_small = ci[method %nin% removed_methods]
saveRDS(ci_aggr_small, here("results", "ci_aggr_small.rds"))
saveRDS(ci_aggr, here("results", "ci_aggr.rds"))
saveRDS(ci_small, here("results", "ci_small.rds"))
saveRDS(ci, here("results", "ci.rds"))

