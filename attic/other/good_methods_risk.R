library(data.table)
library(ggplot2)
library(here)

theme_set(theme_bw())

ci = readRDS(here("results", "ci_aggr.rds"))
ci = ci[measure %in% c("se", "zero_one") & task != "chen_10_null"]
ci[, let(
  rel_width = width_median / .SD[method == "bayle_10_all_pairs", "width_median"][[1L]]
), by = c("task", "size", "learner")]



# small size 

ci1 = ci[
  size %in% c(100, 500) & 
  method %in% c("nested_cv", "conservative_z", "corrected_t_10", "corrected_t_100", "bayle_10_all_pairs")
]



ci1 = ci1[size == 500 & !(task %in% c("video_transcoding", "physiochemical_protein")), ]

# here, we remove the physiochemical_protein and video_transcoding DGPs
# to be ablt to see the rest better

ggplot(ci1[size == 500, ], aes(x = cov_R, y = rel_width, color = method)) + 
  facet_grid(cols = vars(learner)) +
  geom_point(alpha = 0.5, size = 1) + 
  geom_vline(xintercept = 0.95, linetype = "dotted") + 
  labs(
    x = "Relative Coverage Frequency for Risk",
    y = "Median Width relative to Bayle (10)",
    title = "Well-performing methods for n = 500"
  )


ggsave(here("figures", "paper_good_500_risk.png"), width = 8, height = 4)


# Big methods

ci2 = ci[
  size >= 1000 &
  method %in% c("corrected_t_10", "corrected_t_100", "bayle_10_all_pairs", "holdout_90") & 
  !(task %in% c("chen_10_null", "physiochemical_protein", "video_transcoding"))
]

ggplot(ci2[size > 1000, ], aes(x = cov_R, y = rel_width, color = method)) + 
  facet_grid(cols = vars(learner), rows = vars(size)) +
  geom_point(alpha = 0.5, size = 1) + 
  geom_vline(xintercept = 0.95, linetype = "dotted") + 
  labs(
    x = "Relative Coverage Frequency for Risk",
    y = "Median Width relative to Bayle (10)",
    title = "Well-performing methods for n = 10 000"
  )


ggsave(here("figures", "paper_good_10000_risk.png"), width = 8, height = 4)
