library(here)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(ggh4x)
library(ggeasy)
library(ggpubr)

source(here("analysis", "figures", "main", "setup.R"))

theme_set(theme_bw())
sds <- readRDS(here("results", "raw", "sds.rds"))
sds_tbls <- data.table(
  dgp = names(sds),
  sd = unlist(sds)
)


ncv_cheap = readRDS(here("results", "ablation", "ncv_cheap_aggr.rds"))
conz_cheap = readRDS(here("results", "ablation", "conz_cheap_aggr.rds"))
conz_cheap = conz_cheap[inner_reps == 5 & outer_reps == 10,]
cort_cheap = readRDS(here("results", "ablation", "cort_cheap_aggr.rds"))
cort_cheap = cort_cheap[reps == 25,]
complex = readRDS(here("results", "ablation", "complex.rds"))
xgboost = complex[, list(
  ER = ER[[1L]],
  cov_R = mean(lower <= R & upper >= R),
  cov_ER = mean(lower <= ER & upper >= ER),
  iters = iters[[1]],
  median_width = median(width),
  inducer="XGBoost (tuned with 50 evals of BO)",
  task_type=task_type
), by = .(task, loss, method, size)]

mlp_nonaggr = readRDS(here("results", "ablation", "mlp.rds"))
mlp = mlp_nonaggr[, list(
  ER = ER[[1L]],
  cov_R = mean(lower <= R & upper >= R),
  cov_ER = mean(lower <= ER & upper >= ER),
  iters = iters[[1]],
  median_width = median(width),
  inducer="MLP (tuned with 50 evals of BO)",
  task_type=task_type,
  size=5000
), by = .(task, loss, method)]

xgboost <- xgboost %>% 
  rename(dgp=task) %>%
  mutate(method = recode(method, "conz_10_5" = "conz_105",
                         "ncv_3_5" = "ncv_75",
                         "cort_25"="corrected_t_25"))

mlp <- mlp %>% 
  rename(dgp=task) %>%
  mutate(method = recode(method, "conz_10_5" = "conz_105",
                         "ncv_3_5" = "ncv_75",
                         "cort_25"="corrected_t_25"))

#conz_cheap = conz_cheap[inner_reps == 5 & outer_reps == 12,]
conz_cheap$method = "conz_105"
ncv_cheap = ncv_cheap[reps_outer == 3, ]
ncv_cheap$method = "ncv_75"
ncv_cheap$reps_outer = NULL
conz_cheap$inner_reps = NULL
conz_cheap$outer_reps = NULL



#cort_cheap <- cort_aggr[reps==20 & ratio == 0.9,]
cort_cheap$method = "corrected_t_25"

tbl = rbind(conz_cheap, ncv_cheap,cort_cheap,xgboost,mlp,fill=TRUE)

tbl = tbl[size!=100 &
            dgp %nin% susDGPs,
]

tbl$method <- factor(tbl$method, levels=c("conz_105","ncv_75","corrected_t_25"),
                     labels=c("conz_10_5","ncv_3_5","cort_25"))

#tbl$size <- paste0("n = ",tbl$size)


annotate_figure(ggarrange(ggplot(tbl, aes(y = cov_R, color = inducer)) +
                            facet_nested(~size+method) +
                            geom_hline(yintercept = 0.95) +
                            geom_boxplot() +
                            theme(
                              axis.text.x = element_text(angle = 45, hjust = 1),
                              axis.title.x = element_blank()
                            ) + easy_remove_x_axis() +
                            ylab("Coverage of Risk") +
                            scale_color_manual(values = brewer.pal(n = 8, name = "Set1")[-c(6,7)],
                                               labels=c("decision_tree"="Decision Tree",
                                                        "random_forest"="Random Forest",
                                                        "lm_or_logreg"="Linear or Logistic regression",
                                                        "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression")) +
                            labs(color = "Inducer")
                          ,
                          ggplot(tbl, aes(y = cov_ER, color = inducer)) +
                            facet_nested(~size+method, switch = "x") +
                            geom_hline(yintercept = 0.95) +
                            geom_boxplot() +
                            theme(
                              axis.text.x = element_text(angle = 45, hjust = 1),
                              axis.title.x = element_blank()
                            ) + easy_remove_x_axis() +
                            ylab("Coverage of Expected Risk") +
                            scale_color_manual(values = brewer.pal(n = 8, name = "Set1")[-c(6,7)],
                                               labels=c("decision_tree"="Decision Tree",
                                                        "random_forest"="Random Forest",
                                                        "lm_or_logreg"="Linear or Logistic regression",
                                                        "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression")) +
                            labs(color = "Inducer"),
                          nrow=2,common.legend = TRUE
),
bottom=text_grob("conservative z (105 iterations), nested CV (75 iterations), corrected t (25 iterations)\n Data Size")
)

ggsave(here("figures", "additional", "FourthRound_new.png"), width=12,height=5.5)



Widths3 <- tbl %>%
  filter(task_type=="classif")

ggplot(Widths3, aes(y = median_width, color = inducer)) +
  facet_nested(~size+method) +
  geom_boxplot() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + easy_remove_x_axis() +
  ylab("Median width for classification")+
  scale_color_manual(values = brewer.pal(n = 8, name = "Set1")[-c(6,7)],
                     labels=c("decision_tree"="Decision Tree",
                              "random_forest"="Random Forest",
                              "lm_or_logreg"="Linear or Logistic regression",
                              "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression")) +
  labs(color = "Inducer") + theme(legend.position = "bottom")

ggsave(here("figures", "additional", "FourthRound_widths_new.png"), width = 12, height = 4)
