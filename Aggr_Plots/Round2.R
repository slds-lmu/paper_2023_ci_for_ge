source("Aggr_Plots/setup.R")

ci_aggr_red <- ci_aggr[method %nin% c("nested_cv_250","conservative_z_250"),]

Widths <- merge(ci_aggr_red,sds_tbls,by="dgp")

Widths <- Widths[measure %in% translate_losses("Squared", "Zero-One") & 
                   as.character(dgp) %in% setdiff(DGPS,c("adult","video_transcoding","physiochemical_protein","chen_10_null")),
                 list(
                   mean_classif = mean(width[which(task_type=="classif")],na.rm=TRUE),
                   median_classif = median(width[which(task_type=="classif")],na.rm = TRUE),
                   mean_regr = mean(width[which(task_type=="regr")]/sd[which(task_type=="regr")]^2,
                                    na.rm=TRUE),
                   median_regr = median(width[which(task_type=="regr")]/sd[which(task_type=="regr")]^2,
                                        na.rm = TRUE)
                 ),
                 by = c("method")]

Widths <- Widths %>%
  mutate(method = factor(method, levels=levels(UC$method)))


MoI <- setdiff(Widths$method[which(Widths$median_classif<0.2 & Widths$median_regr<0.2)],
               c("ls_bootstrap_100", "ls_bootstrap_50", "ts_bootstrap","bayle_loo"))

p1 <- aggr_plot(ci_aggr_red, MoI, inducers, DGPs, "Squared", "Zero-One", ylims=c(0.65,1)) +
  labs(y = "Average coverage")  +
  theme(legend.position = "none",
        axis.title.y=element_text(size=14)) +
  labs(color = "Inducer", linetype = "Coverage of") +
  scale_color_brewer(palette = "Set1",
                        labels=c("decision_tree"="Decision Tree",
                               "random_forest"="Random Forest",
                               "lm_or_logreg"="Linear or Logistic regression",
                               "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression")) +
  scale_linetype_discrete(labels = c("y_ER"="Expected Risk",
                                     "y_R"="Risk", 
                                     "y_PQ"="Proxy Quantity (if applicable)"))+
  ggtitle("For large data (up to 10.000)")



MoI_small <- setdiff(intersect(Widths$method[which(Widths$median_classif<0.2 & Widths$median_regr<0.4)],
                               setdiff(unique(ci_aggr_red$method),unique(ci_aggr_red[size>500]$method))),
                     c("ls_bootstrap_100", "ls_bootstrap_50", "ts_bootstrap"))

Refac <- ci_aggr_red[which(ci_aggr_red$method %in% MoI_small),] %>%
  mutate(method=factor(method,
                       levels = c("nested_cv","conservative_z","bayle_loo",
                                  "632plus_500","632plus_1000","oob_1000" )
  ))

p2 <- aggr_plot(Refac, MoI_small, inducers, DGPs, "Squared", "Zero-One", ylims=c(0.65,1)) +
  ylab("")  +  
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title.position = "top",
        legend.box.just = "left",
        legend.title = element_text(size=12),
        legend.text = element_text(size=10)) + 
  labs(color = "Inducer", linetype = "Coverage of") +
  guides(color = guide_legend(nrow = 4)) + 
  scale_color_brewer(palette = "Set1",
                        labels=c("decision_tree"="Decision Tree",
                               "random_forest"="Random Forest",
                               "lm_or_logreg"="Linear or Logistic regression",
                               "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression"))+
  scale_linetype_discrete(labels = c("y_ER"="Expected Risk",
                                     "y_R"="Risk", 
                                     "y_PQ"="Proxy Quantity (if applicable)"))+
  ggtitle("For small data (up to 500)")




P2 <- p2+theme(legend.position = "none")
legend <- ggpubr::get_legend(p2)

plot_grid(p1 + theme(strip.text = element_text(margin = margin(1,1,1,1))),
           plot_grid(P2 + theme(strip.text = element_text(margin = margin(1,1,1,1))),
                     legend, ncol = 1, rel_heights = c(1.66,1)))

ggsave("Aggr_Plots/PNGs/SecondRound.png",width=10,height=8)

