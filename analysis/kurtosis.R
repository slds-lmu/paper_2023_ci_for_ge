library(data.table)
library(here)
library(ggplot2)

ci = readRDS(here("results", "ci.rds"))

ci[, let(
  width = upper - lower 
)]
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

ci = ci[method %in% METHODS, ]

ci[, let(
  width_scaled = (width - min(width)) / (diff(range(width)))
), by = c("learner", "task", "measure", "method", "size")]

makeplot = function(.size, .measures) {
  ggplot(ci[measure %in% .measures & size == .size], aes(y = task, x = width_scaled, color = measure)) + 
    facet_grid(rows = vars(method), cols = vars(learner)) + 
    geom_boxplot(alpha = 0.5) + 
    labs(
      x = "Min-Max Scaled Width",
      y = "DGP", 
      title = "Boxplots for Min-Max Scaled Width"
    )
}

makeplot(500, c("se", "ae"))
ggsave(here("figures", "min_max_width_regr_500.pdf"), height = 50, width = 10, limitsize = FALSE)

makeplot(500, c("zero_one", "bbrier", "logloss"))
ggsave(here("figures", "min_max_width_classif_500.pdf"), height = 50, width = 10, limitsize = FALSE)

# Calculate Kurtosis on logscale

tbl = ci[size >= 500, list(
  kurtosis = log10(1 + mean((width - mean(width))^4))
), by = c("task", "learner", "method", "size", "measure")]

ggplot(tbl[size == 500 & task != "chen_10_null" & measure %in% c("se", "zero_one"), ], aes(y = task, x = kurtosis, color = method)) + 
  facet_wrap(vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "DGP",
    title = "Width Distribution, Size = 500, L2 & 0-1 Loss"
  )

ggsave(here("figures", "log_kurtosis_width.png"), width = 8, height = 5)

ggplot(tbl[task != "chen_10_null" & measure %in% c("se", "zero_one"), ], aes(y = task, x = kurtosis, color = method)) + 
  facet_grid(vars(size), vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "DGP",
    title = "Width Distribution, L2 & 0-1 Loss"
  )

ggsave(here("figures", "log_kurtosis_width_full.pdf"), width = 8, height = 25)

ggplot(tbl[size == 500 & task != "chen_10_null" & measure %in% "se", ], aes(y = method, x = kurtosis, color = task)) + 
  facet_wrap(vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Size = 500, Regr, L2"
  )

ggsave(here("figures", "log_kurtosis_width_method_comp_regr.png"), width = 8, height = 5)

ggplot(tbl[task != "chen_10_null" & measure %in% "se", ], aes(y = method, x = kurtosis, color = task)) + 
  facet_grid(vars(size), vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Regr, L2"
  )

ggsave(here("figures", "log_kurtosis_width_method_comp_regr_full.pdf"), width = 8, height = 25)

ggplot(tbl[size == 500 & task != "chen_10_null" & measure %in% "zero_one", ], aes(y = method, x = kurtosis, color = task)) + 
  facet_wrap(vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Size = 500, Classif, 0-1 Loss"
  )

ggsave(here("figures", "log_kurtosis_width_method_comp_classif.png"), width = 8, height = 5)

ggplot(tbl[task != "chen_10_null" & measure %in% "zero_one", ], aes(y = method, x = kurtosis, color = task)) + 
  facet_grid(vars(size), vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Classif, 0-1 Loss"
  )

ggsave(here("figures", "log_kurtosis_width_method_comp_classif_full.pdf"), width = 8, height = 25)

ggplot(tbl[size == 500 & task != "chen_10_null" & measure %in% c("se", "ae", "percentual_se"), ], aes(y = method, x = kurtosis, color = measure)) + 
  facet_wrap(vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Size = 500, Regr"
  )

ggsave(here("figures", "log_kurtosis_width_loss_comp_regr.png"), width = 8, height = 5)

ggplot(tbl[task != "chen_10_null" & measure %in% c("se", "ae", "percentual_se"), ], aes(y = method, x = kurtosis, color = measure)) + 
  facet_grid(vars(size), vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Regr"
  )

ggsave(here("figures", "log_kurtosis_width_loss_comp_regr_full.pdf"), width = 8, height = 25)

ggplot(tbl[size == 500 & task != "chen_10_null" & measure %in% c("zero_one", "bbrier", "logloss"), ], aes(y = method, x = kurtosis, color = measure)) + 
  facet_wrap(vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Size = 500, Classif"
  )

ggsave(here("figures", "log_kurtosis_width_loss_comp_classif.png"), width = 8, height = 5)

ggplot(tbl[task != "chen_10_null" & measure %in% c("zero_one", "bbrier", "logloss"), ], aes(y = method, x = kurtosis, color = measure)) + 
  facet_grid(vars(size), vars(learner)) + 
  geom_point() + 
  geom_jitter() + 
  labs(
    x = "Kurtosis (logscale)", 
    y = "Inference Method",
    title = "Width Distribution, Classif"
  )

ggsave(here("figures", "log_kurtosis_width_loss_comp_classif_full.pdf"), width = 8, height = 25)



