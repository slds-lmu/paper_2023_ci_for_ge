library(ggplot2)
library(data.table)
library(here)
library(plotly)

aggr = readRDS(here("results", "ci_aggr.rds"))


# [1] holdout_66         holdout_90         corrected_t_10     corrected_t_50
# [5] corrected_t_100    bayle_5_within     bayle_5_all_pairs  bayle_10_within
# [9] bayle_10_all_pairs dietterich         oob_10             632plus_10
# [13] oob_50             ls_bootstrap_50    632plus_50         oob_100
# [17] ls_bootstrap_100   632plus_100        nested_cv          conservative_z
# [21] ts_bootstrap       bayle_loo          austern_zhou       austern_zhou_rep
# [25] bccv               bccv_bias          oob_500            oob_1000
# [29] 632plus_1000       632plus_500

synthetic_data = c(
  "bates_classif_20",
  "bates_classif_100",
  "bates_regr_20",
  "bates_regr_100",
  "colon",
  "breast",
  "prostate",
  "chen_10",
  "chen_10_null",
  "friedman1"
  )

plot = function(method_) {
  p = ggplot(aggr[method == method_ & task %in% synthetic_data,], aes(x = size, y = cov_R, color = task)) +
    facet_wrap(vars(learner)) +
    geom_line() +
    geom_hline(yintercept = 0.95, color = "red") +
    labs(
      title = method_
    ) + xlim(50, 10100) + ylim(0, 1)
  ggplotly(p)
}

aggrs = readRDS(here("results", "ci_aggr_small.rds"))

aggrs = aggrs[as.character(aggrs$learner) %in% c("linear", "ridge", "rpart"), ]


aggrs = aggrs[, list(
  avg_cov_R = mean(cov_R)
), by = c("task", "method", "size")]



p = ggplot(aggrs, aes(x = size, y = avg_cov_R, color = task)) +
  facet_wrap(vars(method), scales = "free_x") +
  geom_hline(yintercept = 0.95, color = "red") +
  geom_line() +
  xlim(50, 10500) +
  ylim(0, 1)

ggplotly(p)

