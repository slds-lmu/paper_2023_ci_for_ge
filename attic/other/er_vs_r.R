library(data.table)
library(ggplot2)
library(here)

theme_set(theme_bw())

METHODS = c(
  "holdout_66",
  "holdout_90",
  "corrected_t_10",
  "corrected_t_100",
  "bayle_10_all_pairs",
  "nested_cv",
  "conservative_z"
  # "bayle_loo",
  # "dietterich",
  # "oob_1000",
  # "632plus_1000",
  # "ls_bootstrap_100",
  # "ts_bootstrap"
) 


ci_aggr = readRDS(here("results", "ci_aggr.rds"))

ci_aggr = ci_aggr[method %in% METHODS, ]

ci_aggr = ci_aggr[measure %in% c("se", "zero_one")& !(task %in% c("chen_10_null", "physiochemical_protein", "video_transcoding"))]

ci_aggr$cov_diff = ci_aggr$cov_ER - ci_aggr$cov_R

#ci_aggr = melt(ci_aggr, variable.name = "target", measure.vars = c("cov_R", "cov_ER"), value.name = "coverage")

makeplot = function(.methods, sizes) {
  ggplot(ci_aggr[method %in% .methods & size %in% sizes], aes(y = cov_diff, color = learner)) + 
    facet_grid(vars(size), vars(method), scales = "free_y") + 
    geom_boxplot() + labs(
      y = "Cov(Expected Risk) - Cov(Expected)",
      title = paste0("Coverage of Risk vs. Expected Risk")
    ) + theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}


makeplot(c("bayle_10_all_pairs", "corrected_t_10", "conservative_z", "nested_cv"), c(500, 1000))
ggsave(here("figures", "expected_risk_vs_risk.png"), height = 6, width = 6, limitsize = FALSE)

makeplot(c("nested_cv", "conservative_z", "corrected_t_10", "bayle_10_all_pairs"), 500)


