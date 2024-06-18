library(data.table)
library(ggplot2)
library(here)

theme_set(theme_bw())

METHODS = c(
  "corrected_t_10",
  "bayle_10_all_pairs",
  "conservative_z", 
  "nested_cv"
  # "bayle_loo",
  # "dietterich",
  # "oob_1000",
  # "632plus_1000",
  # "ls_bootstrap_100",
  # "ts_bootstrap"
) 

ci_aggr = readRDS(here("results", "ci_aggr.rds"))
ci_aggr = ci_aggr[method %in% METHODS & measure != "standardized_se", ]
cir = ci_aggr[task_type == "regr"]
cic = ci_aggr[task_type == "classif"]

makeplot = function(.size, task_type) {
  dat = if (task_type == "regr") cir else cic
  ggplot(dat[size == .size & learner %in% c("ranger", "linear"), ], aes(y = cov_R, color = measure)) + 
    facet_grid(vars(learner), vars(method)) + 
    geom_boxplot() + 
    geom_hline(yintercept = 0.95, linetype = "dotted", color = "brown") + 
    labs(
      y = "Coverage (Risk)",
      title = sprintf("Risk Coverage for n = %s for different loss functions", .size)
    )
  
}

makeplot(500, "regr")
ggsave(here("figures", "loss_fns_regr_500.png"), width = 8, height = 4)
makeplot(1000, "regr")
ggsave(here("figures", "loss_fns_regr_1000.png"), width = 8, height = 4)
makeplot(5000, "regr")
ggsave(here("figures", "loss_fns_regr_5000.png"), width = 8, height = 4)
makeplot(10000, "regr")
ggsave(here("figures", "loss_fns_regr_10000.png"), width = 8, height = 4)

makeplot(500, "classif")
ggsave(here("figures", "loss_fns_classif_500.png"), width = 8, height = 4)
makeplot(1000, "classif")
ggsave(here("figures", "loss_fns_classif_1000.png"), width = 8, height = 4)
makeplot(5000, "classif")
ggsave(here("figures", "loss_fns_classif_5000.png"), width = 8, height = 4)
makeplot(10000, "classif")
ggsave(here("figures", "loss_fns_classif_10000.png"), width = 8, height = 4)
