library(data.table)
library(ggplot2)
library(here)

theme_set(theme_bw())

METHODS = c(
  "corrected_t_10",
  "bayle_10_all_pairs"
  # "bayle_loo",
  # "dietterich",
  # "oob_1000",
  # "632plus_1000",
  # "ls_bootstrap_100",
  # "ts_bootstrap"
) 


ci_aggr = readRDS(here("results", "ci_aggr.rds"))

ci_aggr = ci_aggr[size >= 500 & method %in% METHODS & measure %in% c("se", "zero_one") & !(task %in% c("chen_10_null", "physiochemical_protein", "video_transcoding")), ]


ggplot(ci_aggr, aes(x = as.factor(size), y = cov_R, color = method)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(learner)) + 
  geom_hline(yintercept = 0.95) + 
  labs(
    x = "Size", 
    y = "Coverage (Risk)"
  )

ggsave(here("figures", "risk_cov_for_different_n.png"), height = 4, width = 8)

ggplot(ci_aggr, aes(x = as.factor(size), y = cov_ER, color = method)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(learner)) + 
  geom_hline(yintercept = 0.95) + 
  labs(
    x = "Size", 
    y = "Coverage (Expected Risk)"
  ) + 
  ylim(0.7, 1)

ggsave(here("figures", "expected_risk_cov_for_different_n.png"))