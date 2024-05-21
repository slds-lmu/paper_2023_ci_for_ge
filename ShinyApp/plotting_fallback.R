theme_set(theme_bw())

synthetic_data <- c(
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

ci_aggr <- readRDS(here("results", "ci_aggr.rds"))

aggrs_base <- ci_aggr[as.character(ci_aggr$learner) %in% c("linear", "ridge", "rpart"), ]

aggrs <- aggrs_base[, list(
  avg_cov_R = mean(cov_R),
  avg_cov_ER = mean(cov_ER),
  avg_cov_PQ = mean(cov_PQ)
), by = c("task", "method", "size")]


fallback_plot <- function(data, x, y, colorval, method) {
  output <- ggplot(data, aes_string(x = x, y = y, color = colorval)) +
    facet_wrap(vars(method), scales = "free_x") +
    geom_hline(yintercept = 0.95, color = "red") +
    geom_line() +
    xlim(50, 10500) +
    ylim(0, 1) +
    theme_bw()
  return(output)
}


# data=aggrs,x = size,y = avg_cov_R,colorval = task,method=method
# ggplotly(p)

# aggrs_base
method_plot <- function(data, x, y, colorval) {
  ggplot(data, aes_string(x = x, y = y, color = colorval)) +
    facet_wrap(vars(learner), scales = "free_x") +
    geom_hline(yintercept = 0.95, color = "red") +
    geom_line() +
    xlim(50, 10500) +
    ylim(0, 1)
}
