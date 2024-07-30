library(dplyr)
library(data.table)
library(ggplot2)
library(ggeasy)
library(tidyr)

ci <- readRDS("AppendixPlots/ci.rds")

sds <- readRDS("Aggr_Plots/sds.rds")
sds_tbls <- data.table(
  task = names(sds),
  sd = unlist(sds)
)

ci_bayle <- ci[method=="bayle_10_all_pairs" &
                measure %in% c("se","zero_one") & 
                size!=100 & task %in% DGPs &
                learner %in% c("rpart","ranger"),]

ci_bayle$ER_R = ci_bayle$ER-ci_bayle$R
ci_bayle$PQ_ER = ci_bayle$PQ-ci_bayle$ER
ci_bayle$PQ_R = ci_bayle$PQ-ci_bayle$R

ci_bayle <- ci_bayle[PQ_R<=quantile(PQ_R) &
                     PQ_ER<=quantile(PQ_ER) &
                     ER_R<=quantile(ER_R),]
  

ci_bayle_plot <-  merge(ci_bayle,sds_tbls,by="task") %>%
  pivot_longer(cols=c(ER_R,PQ_ER,PQ_R),names_to="difference",values_to="values")

ci_bayle_plot$values[which(ci_bayle_plot$task_type=="regr")] <- ci_bayle_plot$values[which(ci_bayle_plot$task_type=="regr")]/ci_bayle_plot$sd[which(ci_bayle_plot$task_type=="regr")]^2




ggplot(ci_bayle_plot,aes(y=values,color=difference)) +
  geom_boxplot() +
  facet_grid(learner~size) +
  easy_remove_x_axis() +
  theme_bw() + ylim(-0.05,0.05)


sum((ci_bayle$PQ > pmin(ci_bayle$ER, ci_bayle$R)) & (ci_bayle$PQ < pmax(ci_bayle$ER, ci_bayle$R)))/nrow(ci_bayle)*100

