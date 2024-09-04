library(here)
library(data.table)
library(ggplot2)

ci_aggr = readRDS(here("results", "ci_aggr.rds"))

ci_aggr = ci_aggr[grepl(as.character(method), pattern = "^corrected_t")]
ci_aggr[, let(
  repeats = as.factor(as.integer(gsub("^corrected_t_", "", method)))
)]


ci_aggr = dcast(ci_aggr, task + learner + size + measure ~ repeats, value.var = c("cov_R", "width"))

# -> coverage is usually very similar
ci_aggr[, mean(
  abs(cov_R_50 - cov_R_100)
), by = c("task", "learner")]

ci_aggr[, mean(
  width_50 / width_100
), by = c("task", "learner")]

ci_aggr[, mean(cov_R_10 > cov_R_100)]

ci_aggr[, list( 
  rmse_10_50 = sqrt(mean((get("10") - get("50"))^2)),
  rmse_10_199 = sqrt(mean((get("10") - get("100"))^2)),
  rmse_10_50 = sqrt(mean((get("50") - get("10"))^2))
)]
