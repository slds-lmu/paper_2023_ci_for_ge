library(data.table)
library(here)
library(ggplot2)
library(ggh4x)
library(mlr3misc)

theme_set(theme_bw())

ci = readRDS(here("results", "main", "ci.rds"))
ci = ci[method %in% c("holdout_90"), ]
ci[, let(
  width = upper - lower
)]

ci[, let(
  width_scaled = (width - min(width)) / (diff(range(width)))
), by = c("inducer", "dgp", "loss", "method", "size")]


ci$Inducer = map_chr(as.character(ci$inducer), function(l) {
  switch(l,
    lm_or_logreg = "Linear\nRegression",
    random_forest = "Random\nForest",
    decision_tree = "Decision\nTree",
    ridge_lm_or_logreg = "Ridge\nRegression"
  )
})
ci$Loss = ci$loss
ci$Loss = map_chr(as.character(ci$Loss), function(l) {
  switch(l,
    se = "Squared Error",
    winsorized_se = "Winsorized Squared Error",
    l
  )
})

b = ci[size == 10000 & method == "holdout_90" & loss %in% c("se", "winsorized_se"), ]

ggplot(b, aes(y = width_scaled, x = Inducer, color = Loss)) +
  facet_wrap(vars(dgp), nrow = 3) +
  geom_boxplot(outlier.size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    text = element_text(size = 8)
  ) +
  labs(
    x = "Inducer",
    y = "Width (min-max scaled)"
  )

ggsave(here("figures", "appendix", "appendix_boxplot_scaled_width.png"), height = 5, width = 6)
