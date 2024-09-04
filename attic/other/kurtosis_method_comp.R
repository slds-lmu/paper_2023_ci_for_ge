library(data.table)
library(here)
library(ggplot2)

theme_set(theme_bw())

ci = readRDS(here("results", "ci.rds"))

ci[, let(
  width = upper - lower 
)]

methods_small = c(
  "corrected_t_10",
  "corrected_t_100",
  "bayle_10_all_pairs",
  "nested_cv",
  "conservative_z"
) 
methods_big = c(
  "corrected_t_10",
  "holdout_90",
  "corrected_t_100",
  "bayle_10_all_pairs"
)

ggplot(data = tbl[size == 500, ], aes(y = method, x = kurtosis)) + 
  facet_grid(cols = vars(learner)) +
  geom_boxplot()

ggsave(here("figures", "kurtosis_method_comp_500_full.pdf"), height = 30, width = 10, limitsize = FALSE)

tbl = ci[size >= 500, list(
  kurtosis = log10(1 + mean((width - mean(width))^4))
), by = c("task", "learner", "method", "size", "measure")]

ggplot(data = tbl[size == 500 & method %in% methods_small, ], aes(y = method, x = kurtosis)) + 
  facet_grid(cols = vars(learner)) +
  geom_boxplot()

ggsave(here("figures", "kurtosis_method_comp_500.png"), )

ggplot(data = tbl[size == 10000 & method %in% methods_big, ], aes(y = method, x = kurtosis)) + 
  facet_grid(cols = vars(learner)) +
  geom_boxplot()

ggsave(here("figures", "kurtosis_method_comp_10000.png"))

ggplot(data = tbl[size == 10000, ], aes(y = method, x = kurtosis)) + 
  facet_grid(cols = vars(learner)) +
  geom_boxplot()

ggsave(here("figures", "kurtosis_method_comp_10000_full.pdf"), height = 30, width = 10, limitsize = FALSE)

