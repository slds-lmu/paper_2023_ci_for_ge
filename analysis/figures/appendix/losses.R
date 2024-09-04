library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)
library(ggh4x)

theme_set(theme_bw() + theme(text = element_text(size = 10)))

ci_aggr = readRDS(here("results", "ci_aggr.rds"))

# regression

ci_aggr$Loss = ci_aggr$measure
ci_aggr$Learner = ci_aggr$learner
ci_aggr$Learner = map_chr(ci_aggr$Learner, function(l) {
  switch(as.character(l),
    linear = "Linear/Logistic\nRegression",
    ranger = "Random Forest",
    ridge = "Ridge-penalized\nLogistic/Linear Regression",
    rpart = "Decision Tree"
  )
})

ci_aggr$Loss = map_chr(ci_aggr$measure, function(l) {
  switch(as.character(l),
         ae = "AE",
         se = "SE",
         percentual_ae = "Perc.\nAE",
         standardized_ae = "Std.\nAE",
         winsorized_se = "Wins.\nSE",
         bbrier = "Brier",
         logloss = "Log-Loss",
         zero_one = "0-1"
  )
})


regr = ci_aggr[task_type == "regr" & method %in% c("corrected_t_10", "conservative_z", "nested_cv", "holdout_66", "bayle_10_all_pairs")]


dgps = c("chen_10_null", "physiochemical_protein", "video_transcoding")



ggplot(regr[size == 500, ], aes(x = Loss, y = cov_R, color = Learner)) +
  facet_wrap(vars(task)) +
  geom_boxplot(position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0.95) +
  theme(
    legend.position = "top",
  ) + labs(
    y = "Risk Coverage"
  )

ggsave(here("figures", "appendix", "appendix_losses_regr.png"), height = 4, width = 6, dpi = 300)


classif = ci_aggr[task_type == "classif" & method %in% c("corrected_t_10", "conservative_z", "nested_cv", "holdout_66", "bayle_10_all_pairs")]

ggplot(classif[size == 500, ], aes(x = Loss, y = cov_R, color = Learner)) +
  facet_wrap(vars(task)) +
  geom_boxplot(position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0.95, alpha = 0.) +
  theme(
    legend.position = "top"
  ) + labs(
    y = "Risk Coverage"
  )

ggsave(here("figures", "appendix", "appendix_losses_classif.png"), height = 4, width = 6, dpi = 300)
