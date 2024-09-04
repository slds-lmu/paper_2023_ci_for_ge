library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)
library(cowplot)

theme_set(theme_bw())

tbl = readRDS(here("results", "ablation", "cort_aggr.rds"))

tbl = tbl[dgp %nin% c("chen_10_null", "adult", "physiochemical_protein")]

tbl1 = tbl
tbl1$ratio = as.factor(tbl1$ratio)

ggplot(tbl1[task_type == "classif" & ratio == 0.9, ], aes(x = reps, y = sd_width, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner), scales = "free")


ggplot(tbl1[size == 500, ], aes(x = reps, y = cov_R, color = dgp)) + 
  geom_line() +
  facet_grid(vars(ratio), vars(learner)) + labs(
    title = "Size: 500; Coverage by Ratio and Repetitions",
    x = "Repetitions",
    y = "Risk Coverage"
  )


ggsave(here("figures", "ablation", "cort", "overview_cov.png"))

ggplot(tbl1[size == 500 & task_type == "classif", ], aes(x = reps, y = median_width, color = dgp)) + 
  geom_line() +
  facet_grid(vars(ratio), vars(learner)) + labs(
    title = "Size: 500; Coverage by Ratio and Repetitions",
    x = "Repetitions",
    y = "Width"
  )

ggsave(here("figures", "ablation", "cort", "overview_width_classif.png"))

ggplot(tbl1[size == 500 & task_type == "classif", ], aes(x = reps, y = cov_R, color = dgp)) + 
  geom_line() +
  facet_grid(vars(ratio), vars(learner)) + labs(
    title = "Size: 500; Coverage by Ratio and Repetitions",
    x = "Repetitions",
    y = "Risk Coverage"
  )

ggsave(here("figures", "ablation", "cort", "overview_cov_classif.png"))

ggplot(tbl1[size == 500 & task_type == "regr", ], aes(x = reps, y = cov_R, color = dgp)) + 
  geom_line() +
  facet_grid(vars(ratio), vars(learner)) + labs(
    title = "Size: 500; Coverage by Ratio and Repetitions",
    x = "Repetitions",
    y = "Risk Coverage"
  )

ggsave(here("figures", "ablation", "cort", "overview_cov_regr.png"))

