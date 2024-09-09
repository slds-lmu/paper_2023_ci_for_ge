library(here)
library(ggplot2)
library(data.table)
library(mlr3oml)
library(ggdist)
library(mlr3misc)

ci = readRDS(here("results", "main", "ci.rds"))

theme_set(theme_bw() + theme(text = element_text(size = 8)))

ci = ci[size == 500 & loss %in% c("se", "zero_one") &
          method %in% c("cv_10_allpairs", "cort_100", "conz_12_10", "holdout_90", "ncv_10_5"), ]

ci$err = abs(ci$estimate - ci$R)
ci[, let(
  outlier = err > quantile(err, 0.99)
), by = c("method", "loss", "inducer", "dgp", "size")]
ci = ci[(!outlier)]

ggplot(ci[inducer == "lm_or_logreg", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(dgp), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_linear.png"), height = 5, width = 7, dpi = 300)


ggplot(ci[inducer == "random_forest", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(dgp), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_ranger.png"), height = 5, width = 7, dpi = 300)


ggplot(ci[inducer == "decision_tree", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(dgp), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_rpart.png"), height = 5, width = 7, dpi = 300)


ggplot(ci[inducer == "ridge_lm_or_logreg", ], aes(y = err)) +
  geom_violin(aes(fill = method, y = err, x = factor(method))) +
  facet_wrap(vars(dgp), scales = "free", nrow = 4) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.1), legend.key.size = unit(0.5, "cm")) +
  labs(
    y = "Absolute Error of Point Estimate",
    fill = "Method",
    x = element_blank()
  )

ggsave(here("figures", "appendix", "appendix_point_estimates_ridge.png"), height = 5, width = 7, dpi = 300)

