library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)


ncv = readRDS(here("results", "ablation", "ncv_aggr.rds"))

ggplot(data = ncv[learner == "ridge"], aes(x = reps_outer, y = cov_R, color = as.factor(size))) + 
  geom_line() + 
  facet_wrap(vars(dgp), scales = "free")


cort = readRDS(here("results", "ablation", "cort_aggr.rds"))
cort = cort[dgp == "prostate" & ratio == 0.9, ]
ggplot(data = cort, aes(x = reps, y = cov_R, color = as.factor(size))) + 
  geom_line() + 
  facet_grid(vars(learner), vars(dgp))

conz = readRDS(here("results", "ablation", "conz_aggr.rds"))
conz = conz[dgp == "prostate" & inner_reps == 15, ]
ggplot(data = conz, aes(x = outer_reps, y = cov_R, color = as.factor(size))) + 
  geom_line() + 
  facet_grid(vars(learner), vars(dgp))

