aggr = readRDS(here("results", "ci_aggr.rds"))

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


fallback_plot <- function(data,x,y,colorval,method){
output <- ggplot(data, aes_string(x = x, y = y, color = colorval)) +
  facet_wrap(vars(method), scales = "free_x") +
  geom_hline(yintercept = 0.95, color = "red") +
  geom_line() +
  xlim(50, 10500) +
  ylim(0, 1)
return(output)
}

#data=aggrs,x = size,y = avg_cov_R,color = task,method=method
#ggplotly(p)