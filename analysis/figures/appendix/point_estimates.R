library(here)
library(ggplot2)
library(data.table)
library(mlr3oml)
library(ggdist)

ci = readRDS(here("results", "ci.rds"))

theme_set(theme_bw() + theme(text = element_text(size = 8)))

ci = ci[size == 500 & measure %in% c("se", "zero_one") &
          method %in% c("bayle_10_all_pairs", "corrected_t_100", "conservative_z_250", "holdout_90", "nested_cv_250"), ]

ci$err = abs(ci$estimate - ci$R)
ci[, let(
  outlier = err > quantile(err, 0.99)
), by = c("method", "measure", "learner", "task", "size")]
ci = ci[(!outlier)]

ci$method = map_chr(ci$method, function(m) {
  switch(as.character(m),
    bayle_10_all_pairs = "cv_10_allpairs",
    conservative_z_250 = "conz_12_10",
    corrected_t_100 = "cort_100",
    holdout_90 = "holdout_90",
    nested_cv_250 = "ncv_10_5"
  )
})

ggplot(ci[learner == "linear", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(task), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_linear.png"), height = 5, width = 7, dpi = 300)


ggplot(ci[learner == "ranger", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(task), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_ranger.png"), height = 5, width = 7, dpi = 300)


ggplot(ci[learner == "rpart", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(task), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_rpart.png"), height = 5, width = 7, dpi = 300)


ggplot(ci[learner == "ridge", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(task), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_ridge.png"), height = 5, width = 7, dpi = 300)

