library(here)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(ggh4x)
library(ggeasy)
library(ggpubr)

source("Aggr_Plots/setup.R")

theme_set(theme_bw())
sds <- readRDS("Aggr_Plots/sds.rds")
sds_tbls <- data.table(
  dgp = names(sds),
  sd = unlist(sds)
)


ncv_cheap = readRDS("Aggr_Plots/ablation/ncv_cheap_aggr.rds")
conz_cheap = readRDS("Aggr_Plots/ablation/conz_cheap_aggr.rds")
cort_aggr <- readRDS("Aggr_Plots/ablation/cort_aggr.rds")

conz_cheap = conz_cheap[inner_reps == 5 & outer_reps == 12,]
conz_cheap$method = "conz_125"
ncv_cheap = ncv_cheap[reps_outer == 5, ]
ncv_cheap$method = "ncv_125"
ncv_cheap$reps_outer = NULL
conz_cheap$inner_reps = NULL
conz_cheap$outer_reps = NULL


cort_50 <- cort_aggr[reps==50 & ratio == 0.9,]
cort_50$method = "corrected_t_50"

tbl = rbind(conz_cheap, ncv_cheap,cort_50,fill=TRUE)

tbl = tbl[size!=100 &
            dgp %nin% susDGPs,
            ]

tbl$method <- factor(tbl$method, levels=c("conz_125","ncv_125","corrected_t_50"))


annotate_figure(ggarrange(ggplot(tbl, aes(y = cov_R, color = learner)) + 
            facet_nested(~size+method) + 
            geom_hline(yintercept = 0.95) + 
            geom_boxplot() + 
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            ) + easy_remove_x_axis() +
              ylab("Coverage of Risk") +
              scale_color_brewer(palette = "Set1",
                                 labels=c("rpart"="Decision Tree",
                                          "ranger"="Random Forest",
                                          "linear"="Linear or Logistic regression",
                                          "ridge"="Ridge-penalized Linear or Logistic regression")) +
              labs(color = "Inducer")
          ,
          ggplot(tbl, aes(y = cov_ER, color = learner)) + 
            facet_nested(~size+method, switch = "x") + 
            geom_hline(yintercept = 0.95) + 
            geom_boxplot() + 
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            ) + easy_remove_x_axis() +
            ylab("Coverage of Expected Risk") + 
            scale_color_brewer(palette = "Set1",
                               labels=c("rpart"="Decision Tree",
                                        "ranger"="Random Forest",
                                        "linear"="Linear or Logistic regression",
                                        "ridge"="Ridge-penalized Linear or Logistic regression")) +
            labs(color = "Inducer"),
          nrow=2,common.legend = TRUE
),
bottom=text_grob("conservative z (125 repetitions), nested CV (125 repetitions), corrected t (50 repetitions)\n Data Size")
)

ggsave("Aggr_Plots/PNGs/FourthRound.png",width=12,height=5)



Widths3 <- merge(tbl,sds_tbls,by="dgp") 
Widths3$width_of <- NA
Widths3$value <- NA
Widths3$value[which(Widths3$task_type=="classif")] <- Widths3$median_width[which(Widths3$task_type=="classif")]
Widths3$value[which(Widths3$task_type=="regr")] <- Widths3$median_width[which(Widths3$task_type=="regr")]/Widths3$sd[which(Widths3$task_type=="regr")]^2
Widths3$width_of[which(Widths3$task_type=="classif")] <- "median_classif"
Widths3$width_of[which(Widths3$task_type=="regr")] <- "median_regr"

ggplot(Widths3, aes(y = value, color = width_of)) + 
  facet_nested(learner ~ size+method) + 
  geom_hline(yintercept = 0.2) + 
  geom_boxplot() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + easy_remove_x_axis()

