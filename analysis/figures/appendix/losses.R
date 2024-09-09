library(data.table)
library(ggplot2)
library(here)
library(mlr3misc)
library(ggh4x)

theme_set(theme_bw() + theme(text = element_text(size = 10)))

ci_aggr = readRDS(here("results", "main", "ci_aggr.rds"))

# regression

ci_aggr$Loss = ci_aggr$loss
ci_aggr$Learner = ci_aggr$inducer
ci_aggr$Learner = map_chr(ci_aggr$inducer, function(l) {
  switch(as.character(l),
    lm_or_logreg = "Linear/Logistic\nRegression",
    random_forest = "Random Forest",
    ridge_lm_or_logreg = "Ridge-penalized\nLogistic/Linear Regression",
    decision_tree = "Decision Tree"
  )
})

ci_aggr$Loss = map_chr(as.character(ci_aggr$loss), function(l) {
  switch(l,
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


regr = ci_aggr[task_type == "regr" & method %in% c("cort_10", "conz_10_15", "ncv_200_5", "holdout_66", "cv_10_allpairs")]


dgps = c("chen_10_null", "physiochemical_protein", "video_transcoding")



ggplot(regr[size == 500, ], aes(x = Loss, y = cov_R, color = Learner)) +
  facet_wrap(vars(dgp)) +
  geom_boxplot(position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0.95) +
  theme(
    legend.position = "top",
  ) + labs(
    y = "Risk Coverage"
  )

ggsave(here("figures", "appendix", "appendix_losses_regr.png"), height = 4, width = 6, dpi = 300)


classif = ci_aggr[task_type == "classif" & method %in% c("cort_10", "conz_10_15", "ncv_200_5", "holdout_66", "cv_10_allpairs")]

ggplot(classif[size == 500, ], aes(x = Loss, y = cov_R, color = Learner)) +
  facet_wrap(vars(dgp)) +
  geom_boxplot(position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0.95, alpha = 0.) +
  theme(
    legend.position = "top"
  ) + labs(
    y = "Risk Coverage"
  )

ggsave(here("figures", "appendix", "appendix_losses_classif.png"), height = 4, width = 6, dpi = 300)
