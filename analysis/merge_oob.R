library(here)
library(data.table)

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

saveRDS(final, here("results", "final.rds"))