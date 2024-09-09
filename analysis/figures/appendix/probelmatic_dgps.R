library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)
library(ggh4x)

theme_set(theme_bw() + theme(text = element_text(size = 8)))

ci_aggr = readRDS(here("results", "main", "ci_aggr.rds"))

problematic = c("video_transcoding","physiochemical_protein", "chen_10_null")
good_methods = c("cv_10_allpairs", "cort_10", "holdout_66", "holdout_90")
#ci_aggr = ci_aggr[task %in% problematic & method %in% good_methods, ]
ci_aggr = ci_aggr[dgp %in% problematic & loss %in% c("zero_one", "se") & method %in% good_methods, ]

tbl = melt(ci_aggr, id.vars = c("dgp", "size", "inducer", "method", "R_sd", "estimate_sd", "width_median"),
  measure.vars = c("cov_R", "cov_ER"))
setnames(tbl, c("variable", "value"), c("target", "Coverage"))

tbl$Target = as.factor(tbl$target)
tbl$Target = ifelse(tbl$Target == "cov_R", "Risk", "Exp. Risk")
setnames(tbl, "dgp", "DGP")

tbl$inducer = map_chr(as.character(tbl$inducer), function(l) {
  switch(l,
    lm_or_logreg = "Linear / Logistic Regression",
    random_forest = "Random Forest",
    ridge_lm_or_logreg = "Ridge-penalized\nLinear / Logistic Regression",
    decision_tree = "Decision Tree"
  )
})

ggplot(tbl[method %in% c("holdout_90")], aes(x = size, y = Coverage, linetype = Target, color = DGP)) +
  facet_wrap(vars(inducer), nrow = 1) +
  geom_hline(yintercept = 0.95, color = "black") +
  geom_line() +
  labs(
    y = "Coverage",
    x = "Dataset Size",
    linetype = "Target"
  ) +
  theme(legend.position = "top")


ggsave(here("figures", "appendix", "appendix_poor_dgps_cov.png"), dpi = 300, height = 3, width = 6)

ggplot(tbl[method %in% c("cort_10")& Target == "Risk", ], aes(x = as.factor(size), y = log10(1 + R_sd), group = DGP)) +
  facet_grid(vars(DGP), vars(inducer), scales = "free_y") +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + labs(
    y = "SD of Risk (logscale)",
    x = "Dataset Size"
  ) +
  theme(legend.position = "top")

ggsave(here("figures", "appendix", "appendix_poor_dgps_R_sd.png"), dpi = 300, height = 4, width = 6)
