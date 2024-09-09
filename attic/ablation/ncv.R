library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)
library(cowplot)

theme_set(theme_bw())

ncv = readRDS(here("results", "ablation", "ncv_aggr.rds"))
ncv = ncv[dgp %nin% c("chen_10_null", "adult", "physiochemical_protein")]
ncv$size = as.factor(ncv$size)
ggplot(ncv, aes(x = reps_outer, y = cov_R, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner))

ggsave(here("figures", "ablation", "ncv", "overview_cov.png"))

ggplot(ncv, aes(x = reps_outer, y = log(median_width), color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner))

ncv = ncv[, list(
  se_covR = sd(cov_R) / .N,
  est_cov_R = mean(cov_R),
  median_width = mean(median_width)
), by = c("size", "learner", "reps_outer")]

ncv$size = as.factor(ncv$size)


ggplot(data = ncv[size == 500, ], aes(x = reps_outer, y = est_cov_R, color = learner)) + 
  geom_line() + labs(
    x = "Repeats",
    y = "Average Coverage"
  )

ggsave(here("figures", "ablation", "ncv", "aggregated.png"))
  
  
