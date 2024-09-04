library(data.table)
library(here)
library(ggplot2)
library(ggh4x)

theme_set(theme_bw())

ci = readRDS(here("results", "ci.rds"))
ci = ci[method %in% c("conservative_z", "holdout_90", "corrected_t_10", "nested_cv")]
ci[, let(
  width = upper - lower
)]

ci[, let(
  width_scaled = (width - min(width)) / (diff(range(width)))
), by = c("learner", "task", "measure", "method", "size")]


ci$learner = map_chr(ci$learner, function(l) {
  switch(l,
    linear = "Linear\nRegression",
    ranger = "Random\nForest",
    rpart = "Decision\nTree",
    ridge = "Ridge\nRegression"
  )
})
ci$Loss = ci$measure
ci$Loss = map_chr(as.character(ci$Loss), function(l) {
  switch(l,
    se = "Squared Error",
    winsorized_se = "Winsorized Squared Error",
    l
  )
})

ggplot(ci[size == 10000 & method == "holdout_90" & measure %in% c("se", "winsorized_se"), ], aes(y = width_scaled, x = learner, color = Loss)) +
  facet_wrap(vars(task), nrow = 3) +
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
