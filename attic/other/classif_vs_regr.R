library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)
library(ggh4x)

theme_set(theme_minimal() + theme(text = element_text(size = 10)))

ci_aggr = readRDS(here("results", "ci_aggr.rds"))
ci_aggr = ci_aggr[measure %in% c("winsorized_se", "zero_one") & 
  method %in% c("corrected_t_10", "conservative_z", "nested_cv", "holdout_90", "bayle_10_all_pairs"), ]

ggplot(ci_aggr[size == 500, ], aes(x = method, y = cov_R, color = task_type)) + 
  geom_boxplot(position = position_dodge()) + 
  geom_hline(yintercept = 0.95, color = "black") + 
  facet_wrap(vars(learner), nrow = 1) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + 
  labs(
    title = "Classification vs. Regression, size = 500; winsorized SE & 0-1 loss"
  )
