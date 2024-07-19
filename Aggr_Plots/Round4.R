library(here)
library(ggplot2)
library(data.table)
library(mlr3misc)
library(ggh4x)
library(ggeasy)

theme_set(theme_bw())
sds <- readRDS("Aggr_Plots/sds.rds")
sds_tbls <- data.table(
  task = names(sds),
  sd = unlist(sds)
)
tbl = readRDS("Aggr_Plots/ablation/ci_aggr.rds")
#tbl = merge(tbl,sds_tbls,by="task")

tbl = tbl[method %in% c("corrected_t_100", "conservative_z_250", "nested_cv_250") &
            measure %in% c("zero_one", "se") & 
            task %nin% c("adult","video_transcoding","physiochemical_protein","chen_10_null"),
          #list(median_classif = median(width_median[which(task_type=="classif")],na.rm = TRUE),
           #    median_regr = median(width_median[which(task_type=="regr")]/sd[which(task_type=="regr")]^2, na.rm = TRUE))
          ]
tbl$method <- factor(tbl$method,levels=c("conservative_z_250","nested_cv_250","corrected_t_100"))

# tbl = tbl[, list(
#   err = mean(abs(cov_R - 0.95)),
#   cov = mean(cov_R)
# ), by = c("learner", "size", "method")]

annotate_figure(ggarrange(ggplot(tbl, aes(y = cov_R, color = learner)) + 
            facet_nested(~size+method) + 
            geom_hline(yintercept = 0.95) + 
            geom_boxplot() + 
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            ) + easy_remove_x_axis() +
              ylab("Coverage of Risk")
          ,
          ggplot(tbl, aes(y = cov_ER, color = learner)) + 
            facet_nested(~size+method, switch = "x") + 
            geom_hline(yintercept = 0.95) + 
            geom_boxplot() + 
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            ) + easy_remove_x_axis() +
            ylab("Coverage of Expected Risk"),
          nrow=2,common.legend = TRUE
),
bottom=text_grob("conservative z (250), nested CV (250), corrected t (100)\nSize")
)


ggplot(pivot_longer(tbl, cols = all_of(c("median_classif","median_regr")),
                    names_to = "aggr", values_to = "value"), aes(x = method, y = width_median, color = aggr)) + 
  facet_grid(vars(learner), vars(size)) + 
  geom_boxplot() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )
