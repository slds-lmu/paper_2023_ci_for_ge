library(here)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(scales)

theme_set(theme_bw())

ci = readRDS(here("results", "ci.rds"))
ci = ci[method == "bayle_10_all_pairs" & size == 10000 & measure %in% c("se", "zero_one"), ]

ci$pq_err = ci$estimate - ci$PQ
ci$r_err = ci$estimate - ci$R

ci = ci[, list(
  i1 = pq_err <= quantile(pq_err, 0.95) & pq_err >= quantile(pq_err, 0.05),
  i2 = r_err <= quantile(r_err, 0.95) & r_err >= quantile(pq_err, 0.05), pq_err, r_err),
  by = c("task", "learner")]

ci = ci[i1 & i2, ]


ci$learner = map_chr(as.character(ci$learner), function(l) {
  switch(l,
         linear = "Linear / Logistic Regression",
         ranger = "Random Forest",
         ridge = "Ridge-penalized\nLinear / Logistic Regression",
         rpart = "Decision Tree"
  )
})


ggplot(ci[learner != "Ridge-penalized\nLinear / Logistic Regression", ], aes(x = pq_err, y = r_err, color = learner)) +
  facet_wrap(vars(task), nrow = 6, scales = "free") +
  geom_point(alpha = 0.4, size = 0.5) +
  labs(
    x = "Point Estimate - Proxy Quantity",
    y = "Point Estimate - Risk",
    color = "Inducer"
  ) +
  theme(
    legend.key.size = unit(1, "cm"),
    legend.position = "top",
    text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 4))

ggsave(here("figures", "appendix", "appendix_risk_pq.png"), height = 8, width = 7, dpi = 300)

