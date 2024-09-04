library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)
library(cowplot)

theme_set(theme_bw())

conz = readRDS(here("results", "ablation", "conz_aggr.rds"))
ncv = readRDS(here("results", "ablation", "ncv_aggr.rds"))
conz = conz[outer_reps == 50, ]
conz$iters = conz$inner_reps + conz$inner_reps * 2 * conz$outer_reps
conz$method = "conservative_z"
ncv$iters = ncv$reps_outer * 5^2
ncv$method = "nested_cv"

tbl = rbind(conz[, -c("inner_reps", "outer_reps")], ncv[, -c("reps_outer")])
tbl = tbl[dgp %nin% c("chen_10_null", "adult", "physiochemical_protein")]

ggplot(tbl[size == 500, ], aes(x = iters, y = cov_R, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(method), vars(learner))

ggplot(tbl[task_type == "classif" & size == 500, ], aes(x = iters, y = median_width, color = method)) + 
  geom_line() + 
  facet_grid(vars(dgp), vars(learner))

aggr = tbl[size == 500, list(
  width = mean(median_width),
  cov_R = mean(cov_R)
), by = c("size", "learner", "method", "iters", "task_type")]

ggplot(aggr, aes(x = iters, y = cov_R, color = method, linetype = task_type)) + 
  geom_line() + 
  facet_wrap(vars(learner)) +
  labs(
    title = "Average Coverage; ConZ vs. NCV (50 outer reps); n = 500",
    y = "Risk Coverage", 
    x = "Resampling Iterations"
  )
 
ggplot(aggr[task_type == "classif", ], aes(x = iters, y = width, color = method)) + 
  geom_line() + 
  facet_wrap(vars(learner)) + labs(
    title = "Median Width Classification; ConZ vs. NCV (50 outer reps); n = 500",
    y = "Average Median Width",
    x = "Resampling Iterations"
  )
