library(data.table)
library(here)
library(mlr3misc)
library(ggplot2)
library(cowplot)

theme_set(theme_bw())

tbl = readRDS(here("results", "ablation", "conz_aggr.rds"))
tbl = tbl[
  dgp %nin% c("chen_10_null", "adult", "physiochemical_protein") &
  inner_reps %in% c(15, 30)
]

tbl1 = tbl
tbl1$outer_reps = as.factor
tbl1$inner_reps
ggplot(tbl[outer_reps == 50, ], aes(x = inner_reps, y = cov_R, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(learner), vars(size))

ggsave(here("figures", "ablation", "conz", "overview_inner_reps.png"))

tbl$inner_reps = as.factor(tbl$inner_reps)

ggplot(tbl[inner_reps == 15, ], aes(x = outer_reps, y = cov_R, color = dgp)) + 
  geom_line() + 
  facet_grid(vars(size), vars(learner)) + labs(
    title = "Inner Reps: 15",
    x = "Outer Reps",
    y = "Average Coverage"
  )

ggsave(here("figures", "ablation", "conz", "overview_size.png"))
