library(here)
library(ggplot2)
library(data.table)
library(mlr3misc)

theme_set(theme_minimal())
tbl = readRDS(here("results", "ci_aggr.rds"))

tbl = tbl[method %in% c("corrected_t_100", "conservative_z_250", "nested_cv_250") &
  measure %in% c("zero_one", "se") & task %nin% c("chen_10_null", "physiochemical_protein", "adult")]

# tbl = tbl[, list(
#   err = mean(abs(cov_R - 0.95)),
#   cov = mean(cov_R)
# ), by = c("learner", "size", "method")]

ggplot(tbl, aes(x = method, y = cov_R, color = method)) + 
  facet_grid(vars(learner), vars(size), scales = "free") + 
  geom_hline(yintercept = 0.95) + 
  geom_boxplot() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

ggplot(tbl[task_type == "classif", ], aes(x = method, y = width_median, color = method)) + 
  facet_grid(vars(learner), vars(size)) + 
  geom_boxplot() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )



