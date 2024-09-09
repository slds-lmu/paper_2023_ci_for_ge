library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)
library(cowplot)

theme_set(theme_bw())

tbl = readRDS(here("results", "ablation", "cv_aggr.rds"))
tbl = tbl[
  dgp %nin% c("chen_10_null", "adult", "physiochemical_protein")
]

ggplot(tbl[task_type == "classif", ], aes(x = folds, y = sd_width, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner), scales = "free")

  ggplot(tbl, aes(x = folds, y = cov_R, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner))

ggsave(here("figures", "ablation", "cv", "overview_cov.png"))

ggplot(tbl[task_type == "classif", ], aes(x = folds, y = cov_R, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner), scales = "free")

ggsave(here("figures", "ablation", "cv", "overview_cov_classif.png"))

ggplot(tbl[task_type == "classif", ], aes(x = folds, y = median_width, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner), scales = "free")

ggsave(here("figures", "ablation", "cv", "overview_width_classif.png"))
