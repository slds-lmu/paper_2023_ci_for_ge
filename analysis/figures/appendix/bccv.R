library(data.table)
library(here)
library(ggplot2)
library(mlr3misc)

theme_set(theme_bw())

tbl = readRDS(here("results", "ci_aggr.rds"))

tbl = tbl[method %in% c("bccv", "bccv_bias") & measure %in% c("zero_one", "se"), ]

tbl = tbl[, c("task", "learner", "size", "cov_R", "cov_ER", "width_median", "method")]

tbl = melt(tbl, measure.vars = c("cov_R", "cov_ER"))
setnames(tbl, c("variable", "value"), c("Target", "Coverage"))

tbl$Inducer = map_chr(tbl$learner, function(l) {
  switch(l,
         linear = "Linear/Logistic\nRegression",
         rpart = "Decision Tree",
         ranger = "Random Forest",
         ridge = "Ridge-penalized\nLinear/Logistic Regression"
  )
})

tbl$Target = map_chr(tbl$Target, function(t) {
  switch(t, cov_R = "Risk", cov_ER = "Expected Risk")
})

ggplot(tbl[Target == "Risk", ], aes(x = Coverage, y = task, color = Inducer)) +
  facet_wrap(vars(method), nrow = 1) +
  geom_point() +
  geom_vline(xintercept = 0.95, color = "black") +
  theme(
    legend.position = "top"
  ) +
  labs(shape = "Target", color = "Method", y = "DGP", x = "Risk Coverage")

ggsave(here("figures", "appendix", "appendix_bccv.png"), dpi = 300, width = 8, height = 4)
